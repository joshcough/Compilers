package L5Compiler

import L5AST._

object L5CompilerMain extends L5Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L5Compiler extends io.Reader with L5Parser with L5Printer {

  def compile(code:String): E = {
    val ast = parse(read(code))
    ast
  }

  def compileToString(code:String) = L5Printer.toCode(compile(code))

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
}