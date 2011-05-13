package L5Compiler

import L5AST._

import L4Compiler.L4AST
import L4Compiler.L4Printer
import L4Compiler.L4AST.{
  ClosureVars, ClosureProc, L4, E => L4E, Func, MakeClosure, Label,
  NewTuple => L4NewTuple, Variable => L4Variable, Let => L4Let, ARef => L4Aref, Num => L4Num, IfStatement => L4If}

object L5CompilerMain extends L5Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L5ToL4Implicits {
  implicit def convertVar(v:Variable) = L4Variable(v.name)
}

trait L5Compiler extends io.Reader with L5Parser with L5Printer with L5ToL4Implicits{

  def compile(code:String): L4 = {
    val ast = parse(read(code))
    val (e,fs) = compile(ast)
    L4(e, fs)
  }

  def compileToString(code:String) = L4Printer.toCode(compile(code))

  def compileEs(es:List[E]): (List[L4E], List[Func]) = {
    val (l4es, funs) = es.map(compile).unzip
    (l4es, funs.flatten)
  }

  /**
    e ::= (lambda (x ...) e) *
    | x *
    | (let ([x e]) e) TODO
    | (letrec ([x e]) e) TODO
    | (if e e e) TODO
    | (new-tuple e ...) TODO
    | (begin e e) TODO
    | (e e ...) ;; application expression *
    | prim *
    | num *
   */
  def compile(e:E): (L4E, List[Func]) = e match {
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
      val label = newLabel()
      val frees = freeVars(e)
      val vars = L4Variable("frees")
      val fArgsV = L4Variable("args")
      val fArgs: List[L4Variable] =
        (if(frees.isEmpty) Nil else List(vars)) :::
        (if(args.size + frees.size <= 3) args.map(convertVar) else List(fArgsV))
      val (fBody, moreFunctions) = {
        val (inner, funcs) = compile(body)
        val freeLets = if(frees.isEmpty) inner else {
          frees.zipWithIndex.foldRight(inner){ case ((v,i), b) => L4Let(v, L4Aref(vars, L4Num(i)), b) }
        }
        val finalLets = if(args.size + frees.size <= 3) freeLets else {
          args.zipWithIndex.foldRight(freeLets){ case ((v,i), b) => L4Let(v, L4Aref(fArgsV, L4Num(i)), b) }
        }
        (finalLets, funcs)
      }
      val closure =
        if(frees.isEmpty) MakeClosure(label, L4Num(1))
        else MakeClosure(label, L4NewTuple(frees.map(convertVar)))
      (closure, Func(label, fArgs, fBody) :: moreFunctions)
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
      (p match {
        case Print => L4AST.Print(l4es(0))
        case NewArray => L4AST.NewArray(l4es(0), l4es(1))
        case ARef => L4AST.ARef(l4es(0), l4es(1))
        case ASet => L4AST.ASet(l4es(0), l4es(1), l4es(2))
        case ALen => L4AST.ALen(l4es(0))
        case Add => L4AST.Add(l4es(0), l4es(1))
        case Sub => L4AST.Sub(l4es(0), l4es(1))
        case Mult => L4AST.Mult(l4es(0), l4es(1))
        case LessThan => L4AST.LessThan(l4es(0), l4es(1))
        case LessThanOrEqualTo => L4AST.LessThanOrEqualTo(l4es(0), l4es(1))
        case EqualTo => L4AST.EqualTo(l4es(0), l4es(1))
        case IsNumber => L4AST.IsNumber(l4es(0))
        case IsArray => L4AST.IsArray(l4es(0))
      }, moreFuns)
    }

    case App(f, args) => {
      val v = newVar()
      val (compiledF, extraFunctions) = compile(f)
      val (compiledArgs, moreExtraFunctions) = compileEs(args)
      (L4Let(v, compiledF, {
        // if a number is in the closures argument position
        // then we know it didnt have any free variables.
        L4If(L4AST.IsNumber(L4AST.ClosureVars(v)),
          // so we can call it with just the arguments
          L4AST.FunCall(ClosureProc(v),
            // if we can fit those arguments in, then great
            if(compiledArgs.size <= 3) compiledArgs
            // if not, create a new tuple and
            else List(L4NewTuple(compiledArgs))
          ),
          // the function must have had free variables, so those must go in the first argument.
          L4AST.FunCall(ClosureProc(v), ClosureVars(v) ::
            // if we can fit the rest of the arguments in, then great
            (if(compiledArgs.size <= 2) compiledArgs
            // if not, they must also go into another tuple.
            else List(L4NewTuple(compiledArgs)))
          )
        )
      }), extraFunctions ::: moreExtraFunctions)
    }

    // (f +) => (f (lambda (x y) (+ x y)))
    case p:Prim => {
      val vars = List("x", "y", "z").map(Variable(_)).take(nrArgs(p))
      compile(
        Lambda(vars, p match {
          case Print => App(Print, vars)
          case NewArray => App(NewArray,vars)
          case ARef => App(ARef, vars)
          case ASet => App(ASet, vars)
          case ALen => App(ALen, vars)
          case Add => App(Add, vars)
          case Sub => App(Sub, vars)
          case Mult => App(Mult, vars)
          case LessThan => App(LessThan, vars)
          case LessThanOrEqualTo => App(LessThanOrEqualTo, vars)
          case EqualTo => App(EqualTo, vars)
          case IsNumber => App(IsNumber, vars)
          case IsArray => App(IsArray, vars)
        })
      )
    }
    case Num(n) => (L4Num(n), Nil)
    case Variable(v) => (L4Variable(v), Nil)
    case _ => error("implement me: " + e)
  }

  private val count = Iterator.from(0)
  def newLabel() = Label("f" + count.next())
  def newVar() = L4Variable("x" + count.next())

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

  def nrArgs(p: Prim) = p match {
    case Print => 1
    case ALen => 1
    case IsNumber => 1
    case IsArray => 1
    case ASet => 3
    case NewArray => 2
    case ARef => 2
    case _ => 2
  }
}

//  def sub(e:E, oldE:E, newE:E): E = {
//    def inner(e:E): E = e match {
//      case l@Lambda(args, body) => Lambda(args, if(args.contains(oldE)) body else inner(body))
//      case v: Variable => if (oldE == v) newE else v
//      case Let(x, r, body) => Let(x, inner(r), if (oldE == x) body else inner(body))
//      // TODO: LetRec
//      //case LetRed(x, r, body) => Let(x, inner(r), if (oldE == x) body else inner(body))
//      case IfStatement(e, t, f) => IfStatement(inner(e), inner(t), inner(f))
//      case Begin(e1, e2) => Begin(inner(e1), inner(e2))
//      case NewTuple(es) => NewTuple(es.map(inner))
//      //(+ x y z)  => (+ x y z)
//      case App(p:Prim, args) => App(p, args.map(inner))
//      case App(f, args) => App(inner(f), args.map(inner))
//      //(f +) => (f (lambda (x y) (+ x y)))
//      case p:Prim => if (oldE == p) newE else p
//      case n:Num => n
//    }
//    inner(e)
//  }
