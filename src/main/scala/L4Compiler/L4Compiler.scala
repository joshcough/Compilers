package L4Compiler

import L4AST._

trait L4Compiler extends io.Reader with L4Parser with L4Printer with FunctionLifters {

  def compileE(code:String) = L4Printer.toCode(find(parseE(read(code)), NoContext))
  def isV(e:E): Boolean = e.isInstanceOf[V]

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class NAryContext(vs:List[V], rest:List[E], f: List[V] => E, k:Context) extends Context
  case object NoContext extends Context

  // find: L4-e context -> L3-e
  def find(e:E, k:Context): E = e match {
    case Let(x, r, body) => find(r, LetContext(x, body, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case e: EN => find(e.first, NAryContext(Nil, e.rest, e.rebuild, k))
    case v:V => fill(e, k)
  }

  // fill: L3-d context -> L3-e
  def fill(d:E, k:Context): E = k match {
    case LetContext(v, b, k) => Let(v, d, find(b, k))
    case IfContext(t, e, k) => next(d, k, v => IfStatement(v, find(t, k), find(e, k)))
    case NAryContext(vs, rest, f, k) => rest match {
      case Nil => next(d, k, v => fill(f(vs :+ v), k))
      case (x::xs) => next(d, k, v => find(x, NAryContext(vs :+ v, xs, f, k)))
    }
    case NoContext => d
  }

  def next(d:E, k:Context, f: V => E): E = {
    if(isV(d)) f(d.asInstanceOf[V])
    else { val x = newVar(); Let(x, d, f(x)) }
  }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())
}

trait FunctionLifters {
  def lift1[T, U](f: T => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::Nil => f(x)
    case _ => error("expected 1 arg, but got: " + ts)
  }

  def lift2[T, U](f: (T, T) => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::y::Nil => f(x, y)
    case _ => error("expected 2 args, but got: " + ts)
  }

  def lift3[T, U](f: (T, T, T) => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::y::z::Nil => f(x, y, z)
    case _ => error("expected 3 args, but got: " + ts)
  }
}