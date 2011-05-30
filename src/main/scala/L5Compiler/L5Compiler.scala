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

trait L5Compiler extends io.Reader with L5Parser with L5Printer {
  //  L5 -> L4 Implicit Conversions
  implicit def convertVar(v:Variable) = L4.Variable(v.name)
  implicit def convertVarList(vs:List[Variable]) = vs.map(convertVar)
  
  def compileToString(code:String) = L4Printer.toCode(compile(code))
  def compile(code:String): L4.L4 = {
    val (e,fs) = compile(parse(read(code)))
    L4.L4(e, fs)
  }

  def compile(e:E): (L4.E, List[L4.Func]) = e match {
    case Num(n) => (L4.Num(n), Nil)
    case v:Variable => (v, Nil)
    case IfStatement(e, t, f) => compileMore(e,t,f){ es => L4.IfStatement(es(0), es(1), es(2)) }
    case Begin(e1, e2) => compileMore(e1,e2){ es => L4.Begin(es(0), es(1)) }
    case NewTuple(es) =>  compileMore(es:_*){ es => L4.NewTuple(es) }
    case Let(x, e1, e2) => compileMore(e1,e2){ es => L4.Let(x, es(0), es(1)) }
    // (f +) => (f (lambda (x y) (+ x y)))
    case p:Prim => compile(Lambda(p.vars, App(p, p.vars)))
    // (+ x y z)  => (+ x y z)
    case App(p:Prim, args) => compileMore(args:_*){ es => L4.FunCall(L4.keywordsMap(p.name), es) }
    case App(f, args) => {
      val v = newVar()
      val (compiledF, extraFunctions) = compile(f)
      val (compiledArgs, moreExtraFunctions) = compileEs(args)
      (L4.Let(v, compiledF,
        // free variables go in the first argument.
        L4.FunCall(L4.ClosureProc(v), L4.ClosureVars(v) ::
        // if we can fit the rest of the arguments in, then great
        // if not, they must also go into another tuple.
        (if (compiledArgs.size <= 2) compiledArgs else List(L4.NewTuple(compiledArgs))))),
      extraFunctions ::: moreExtraFunctions)
    }
    case LetRec(x, e1, e2) => compile(
      Let(x, NewTuple(List(Num(0))),
        Begin(
          App(ASet, List(x, Num(0), sub(x, e1, App(ARef, List(x, Num(0)))))),
          sub(x, e2, App(ARef, List(x, Num(0))))
        )
      )
    )
    case Lambda(args, body) => {
      /**
        (lambda (x ...) e) => (make-closure :f (new-tuple y1 y2 ... y-n))
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
        def fold(tup:L4.Variable, vars:List[L4.Variable], init:L4.E) =
          vars.zipWithIndex.foldRight(init){ case ((v,i), b) => L4.Let(v, L4.ARef(tup, L4.Num(i)), b) }
        val freeLets = fold(freesVar, frees, inner)
        val finalLets = if(! usingArgsTuple) freeLets else fold(argsVar, args, freeLets)
        (finalLets, funcs)
      }
      val label = newLabel()
      (L4.MakeClosure(label, L4.NewTuple(frees)), L4.Func(label, fArgs, fBody) :: moreFunctions)
    }
  }

  def compileMore(es:E*)(f: List[L4.E] => L4.E) = {
    val (l4es, fs) = compileEs(es.toList)
    (f(l4es), fs)
  }

  def compileEs(es:List[E]): (List[L4.E], List[L4.Func]) = {
    val (l4es, fs) = es.map(compile).unzip
    (l4es, fs.flatten)
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
}