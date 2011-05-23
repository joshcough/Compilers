package L5Compiler

import L5AST._

import L4Compiler.{ L4AST => L4 }
import L4Compiler.L4Printer

object L5CompilerMain extends L5Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L5ToL4Implicits {
  implicit def convertVar(v:Variable) = L4.Variable(v.name)
  implicit def convertVarList(vs:List[Variable]) = vs.map(convertVar)
}

trait L5Compiler extends io.Reader with L5Parser with L5Printer with L5ToL4Implicits{

  def compile(code:String): L4.L4 = {
    val ast = parse(read(code))
    val (e,fs) = compile(ast)
    L4.L4(e, fs)
  }

  def compileToString(code:String) = L4Printer.toCode(compile(code))

  def compileEs(es:List[E]): (List[L4.E], List[L4.Func]) = {
    val (l4es, funs) = es.map(compile).unzip
    (l4es, funs.flatten)
  }

  def compile(e:E): (L4.E, List[L4.Func]) = e match {
    case Lambda(args, body) => {
      /**
        Specifically, if we see
          (lambda (x ...) e)
        in the program, we replace it with
          (make-closure :f (new-tuple y1 y2 ... y-n))
        where (y1 y2 ... y-n) are the free variables in (lambda (x ...) e),
        and we create a new procedure:
          (:f (vars-tup x ...)
              (let ([y1 (aref vars-tup 0)])
                (let ([y2 (aref vars-tup 1)])
                  ...
                  (let ([y-n (aref vars-tup n)])
                    e))))
      */
      val frees = freeVars(e)
      val usingArgsTuple = args.size > 2
      val (freesVar, argsVar) = (L4.Variable("frees"), L4.Variable("args"))
      val fArgs: List[L4.Variable] =
        if(usingArgsTuple) List(freesVar, argsVar) else freesVar :: args.map(convertVar)

      val (fBody, moreFunctions) = {
        val (inner, funcs) = compile(body)
        val freeLets =
          frees.zipWithIndex.foldRight(inner){ case ((v,i), b) => L4.Let(v, L4.ARef(freesVar, L4.Num(i)), b) }
        val finalLets = if(! usingArgsTuple) freeLets else {
          args.zipWithIndex.foldRight(freeLets){ case ((v,i), b) => L4.Let(v, L4.ARef(argsVar, L4.Num(i)), b) }
        }
        (finalLets, funcs)
      }

      val label = newLabel()
      (L4.MakeClosure(label, L4.NewTuple(frees)), L4.Func(label, fArgs, fBody) :: moreFunctions)
    }
    case App(p:Prim, args) => {
      assert(args.size == nrArgs(p))
      /**
       - when a primitive operation shows up in the function position of an
         application, we need to just leave it there. But when it shows up
         in some other place, we just turn it into lambda expression and
         then closure convert it. For example:
          (+ x y z)  => (+ x y z)
          (f +) => (f (lambda (x y) (+ x y)))
       */
      val (l4es, moreFuns) = compileEs(args)
      (L4.FunCall(L4.keywordsMap(p.name), l4es), moreFuns)
    }
    case App(f, args) => {
      val v = newVar()
      val (compiledF, extraFunctions) = compile(f)
      val (compiledArgs, moreExtraFunctions) = compileEs(args)
      (L4.Let(v, compiledF,
        // free variables go in the first argument.
        L4.FunCall(L4.ClosureProc(v), L4.ClosureVars(v) ::
        // if we can fit the rest of the arguments in, then great
        (if (compiledArgs.size <= 2) compiledArgs
        // if not, they must also go into another tuple.
        else List(L4.NewTuple(compiledArgs))))),
      extraFunctions ::: moreExtraFunctions)
    }
    // (f +) => (f (lambda (x y) (+ x y)))
    case p:Prim => {
      val vars = List("x", "y", "z").map(Variable(_)).take(nrArgs(p))
      compile(Lambda(vars, App(p, vars)))
    }
    case IfStatement(e, t, f) => {
      val (es, fs) = compileEs(List(e,t,f))
      (L4.IfStatement(es(0), es(1), es(2)), fs)
    }
    case Begin(e1, e2) => {
      val (es, fs) = compileEs(List(e1,e2))
      (L4.Begin(es(0), es(1)), fs)
    }
    case NewTuple(es) => {
      val (compiledEs, fs) = compileEs(es)
      (L4.NewTuple(compiledEs), fs)
    }
    case Let(x, e1, e2) => {
      val (es, fs) = compileEs(List(e1,e2))
      (L4.Let(x, es(0), es(1)), fs)
    }
    case LetRec(x, e1, e2) => compile(
      Let(x, NewTuple(List(Num(0))),
        Begin(
          App(ASet, List(x, Num(0), sub(x, e1, App(ARef, List(x, Num(0)))))),
          sub(x, e2, App(ARef, List(x, Num(0))))
        )
      )
    )
    case Num(n) => (L4.Num(n), Nil)
    case v:Variable => (v, Nil)
  }

  private val labelCount = Iterator.from(0)
  private val varCount = Iterator.from(0)
  def newLabel() = L4.Label("f" + labelCount.next())
  def newVar() = L4.Variable("x" + varCount.next())

  def freeVars(e:E): List[Variable] = {
    def inner(e:E, bound:List[Variable]): List[Variable] = e match {
      case Lambda(args, body) => inner(body, args ::: bound)
      case v:Variable => bound.find(_==v) match { case None => List(v); case _ => Nil}
      case Let(x, r, body) => inner(r, bound) ::: inner(body, x :: bound)
      case LetRec(x, r, body) => inner(r, x :: bound) ::: inner(body, x :: bound)
      case IfStatement(e, t, f) => inner(e, bound) ::: inner(t, bound) ::: inner(f, bound)
      case Begin(e1, e2) => inner(e1, bound) ::: inner(e2, bound)
      case NewTuple(es) => es.flatMap(e => inner(e, bound))
      case App(f, args) => inner(f, bound) ::: args.flatMap(e => inner(e, bound))
      case _ => Nil
    }
    inner(e, Nil).distinct
  }

  def sub(x:Variable, replacee:E, replacer:E): E = {
    def inner(e:E): E = e match {
      case Lambda(args, body) => Lambda(args, if(args.contains(x)) body else inner(body))
      case v:Variable => if(v==x) replacer else v
      case Let(v, e1, body) => Let(v, inner(e1), if(v==x) body else inner(body))
      case LetRec(v, e1, body) => LetRec(v, if(v==x) e1 else inner(e1), if(v==x) body else inner(body))
      case IfStatement(e, t, f) => IfStatement(inner(e), inner(t), inner(f))
      case Begin(e1, e2) => Begin(inner(e1), inner(e2))
      case NewTuple(es) => NewTuple(es.map(inner))
      case App(f, args) => App(inner(f), args.map(inner))
      case _ => e
    }
    inner(replacee)
  }

  def nrArgs(p: Prim) = p match {
    case Print | ALen | IsNumber | IsArray => 1
    case ASet => 3
    case _ => 2
  }
}