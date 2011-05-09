package L4Compiler

import L4AST._

trait L4Compiler extends io.Reader with L4Parser with L4Printer {

  def compile(code:String) = {
    val ast = parse(read(code))
    L4Printer.toCode(L4(find(ast.main), ast.funs.map(find)))
  }

  def compileE(code:String) = L4Printer.toCode(find(parseE(read(code))))
  def isV(e:E): Boolean = e.isInstanceOf[V]

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class NAryContext(vs:List[V], rest:List[E], f: List[V] => E, k:Context) extends Context
  case object NoContext extends Context

  def find(f:Func): Func = f.copy(body=find(f.body))
  def find(e:E): E = find(e, NoContext)

  // find: L4-e context -> L3-e
  def find(e:E, k:Context): E = e match {
    case Let(x, r, body) => find(r, LetContext(x, body, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case Begin(e1, e2) => find(Let(newVar(), e1, e2), k)
    case e: EN => find(e.first, NAryContext(Nil, e.rest, e.rebuild, k))
    case v:V => fill(e, k)
  }

  // fill: L3-d context -> L3-e
  def fill(d:E, k:Context): E = k match {
    case LetContext(v, b, k) => Let(v, d, find(b, k))
    case IfContext(t, e, k) => rebuild(d, k, v => IfStatement(v, find(t, k), find(e, k)))
    case NAryContext(vs, rest, f, k) => rest match {
      case Nil => rebuild(d, k, v => fill(f(vs :+ v), k))
      case (x::xs) => rebuild(d, k, v => find(x, NAryContext(vs :+ v, xs, f, k)))
    }
    case NoContext => d
  }

  def rebuild(d:E, k:Context, f: V => E): E = {
    if(isV(d)) f(d.asInstanceOf[V]) else { val x = newVar(); Let(x, d, f(x)) }
  }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())
}