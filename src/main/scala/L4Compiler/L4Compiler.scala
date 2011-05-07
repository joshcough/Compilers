package L4Compiler

import L4AST._

trait L4Compiler extends io.Reader with L4Parser with L4Printer {

  def compileE(code:String) = L4Printer.toCode(find(parseE(read(code)), NoContext))

  def isV(e:E): Boolean = e.isInstanceOf[V]

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class FunContext(args:List[E], k:Context) extends Context
  case class ArgContext(f:V, k:Context) extends Context
  case class E2LeftContext(r:E, f: (V, V) => E, k:Context) extends Context
  case class E2RightContext(l:V,  f: (V, V) => E, k:Context) extends Context
  case class E1Context(f: V => E, k:Context) extends Context
  case object NoContext extends Context

  // NOTE: kind of terrible that the return val has to be an L4 E here...
  // find: L4-e context -> L3-e
  def find(e:E, k:Context): E = e match {
    case FunCall(f, args) => find(f, FunContext(args, k))
    case Let(x, r, body) => find(r, LetContext(x, r, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case e: E1 => find(e.e1, E1Context(e.rebuild, k))
    case e: E2 => find(e.e1, E2LeftContext(e.e2, e.rebuild, k))
    case v:V => fill(e, k)
    // TODO: other cases here too... lots.
    case _ =>  error("implement me")
  }

  // NOTE: kind of terrible that the first arg has to be an L4 E here...
  // NOTE: kind of terrible that the return val has to be an L4 E here...
  // fill: L3-d context -> L3-e
  def fill(d:E, k:Context): E = k match {
    case LetContext(v, b, k) => Let(v, d, find(b, k))
    case IfContext(t, e, k) => next(d, k, v => IfStatement(v, find(t, k), find(e, k)))
    // TODO: only handling one argument.
    case FunContext(as, k) => next(d, k, v => find(as.head, ArgContext(v, k)))
    // TODO: i think this is all busted because a single argument
    // could have more after it. maybe we need a new context type like
    // LastArgContext which would do this work, and ... something that
    // comes before LastArg...
    // TODO obviously List(e) doesnt really work here. see comment above.
    case ArgContext(f, k) => next(d, k, v => fill(FunCall(f, List(v)), k))
    case E1Context(f, k) => next(d, k, v => fill(f(v), k))
    case E2LeftContext(r, f, k) => next(d, k, v => find(r, E2RightContext(v, f, k)))
    case E2RightContext(l, f, k) => next(d, k, v => fill(f(l, v), k))
    case NoContext => d
  }

  def next(d:E, k:Context, f: V => E): E = {
    if(isV(d)) f(d.asInstanceOf[V])
    else {
      val x = newVar()
      Let(x, d, f(x))
    }
  }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())
}