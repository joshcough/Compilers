package L4Compiler

object L4AST extends FunctionLifters {

  sealed trait L4ASTNode
  //p::= (e (l (x ...) e) ...)
  object L4{ def apply(main: E): L4 = L4(main, Nil) }
  case class L4(main: E, funs:List[Func]) extends L4ASTNode
  case class Func(label:Label, args:List[Variable], body: E) extends L4ASTNode

  sealed trait E extends L4ASTNode
  trait EN extends E { def es: List[E]; def rebuild: (List[E]) => E }
  sealed trait E1 extends EN{
    def e: E; def es = List(e); def rebuild1: E => E; def rebuild = lift1(rebuild1)
  }
  sealed trait E2 extends EN {
    def e1: E; def e2: E; def es = e1 :: List(e2)
    def rebuild2: (E, E) => E; def rebuild = rebuild2
  }
  case class Let(v:Variable, e:E, body:E) extends E
  case class IfStatement(e:E, truePath:E, falsePath:E) extends EN {
    def es = List(e, truePath, falsePath); def rebuild = lift3(IfStatement.apply _)
  }
  case class Begin(e1:E, e2:E) extends E2{ val rebuild2 = Begin.apply _}
  case class FunCall(f:E, args:List[E]) extends EN {
    def es = f :: args; def rebuild = (es:List[E]) => new FunCall(es.head, es.tail)
  }

  case class Add(e1:E, e2:E) extends E2{ val rebuild2 = Add.apply _ }
  case class Sub(e1:E, e2:E) extends E2{ val rebuild2 = Sub.apply _ }
  case class Mult(e1:E, e2:E) extends E2{ val rebuild2 = Mult.apply _ }
  case class LessThan(e1:E, e2:E) extends E2{ val rebuild2 = LessThan.apply _ }
  case class LessThanOrEqualTo(e1:E, e2:E) extends E2{ val rebuild2 = LessThanOrEqualTo.apply _ }
  case class EqualTo(e1:E, e2:E) extends E2{ val rebuild2 = EqualTo.apply _ }

  case class IsNumber(e:E) extends E1{ val rebuild1 = IsNumber.apply _ }
  case class IsArray(e:E) extends E1{ val rebuild1 = IsArray.apply _ }

  case class NewArray(size:E, init:E) extends E2 {
    val e1 = size; val e2 = init; val rebuild2 = NewArray.apply _
  }
  case class NewTuple(vs:List[E]) extends EN { def es = vs; def rebuild = NewTuple.apply _ }
  case class ARef(arr:E, loc:E) extends E2 { val e1 = arr; val e2 = loc; val rebuild2 = ARef.apply _ }
  case class ASet(arr:E, loc:E, newVal: E) extends EN{
    val es = List(arr, loc, newVal); val rebuild = lift3(ASet.apply _)
  }
  case class ALen(arr:E)  extends E1{ val e = arr; val rebuild1 = ALen.apply _ }
  case class MakeClosure(l:Label, e:E) extends E1{ val rebuild1 = (e:E) => MakeClosure.apply(l, e) }
  case class ClosureProc(e:E) extends E1{ val rebuild1 = ClosureProc.apply _ }
  case class ClosureVars(e:E) extends E1{ val rebuild1 = ClosureVars.apply _ }

  case class Print(e:E) extends E1{ val rebuild1 = Print.apply _ }

  trait V extends E
  case class Variable(name: String) extends V
  case class Num(n: Int) extends V
  case class Label(name: String) extends V {
    override def toString = "Label(\"" + name + "\")"
  }


  def sub(x:Variable, replacee:E, replacer:E): E = replacee match {
    case v:Variable => if(v == x) replacer else v
    case Let(v, r, body) => Let(v, sub(x, r, replacer), if(v==x) body else sub(x, body, replacer))
    case en: EN =>  en.rebuild(en.es.map(e => sub(x, e, replacer)))
    case v:V => v // number or label. variable was handled above.
  }
}

trait FunctionLifters {
  def lift1[T, U](f: T => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::Nil => f(x)
    case _ => error("expected 1 arg, but got: " + ts)
  }
  implicit def lift2[T, U](f: (T, T) => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::y::Nil => f(x, y)
    case _ => error("expected 2 args, but got: " + ts)
  }
  def lift3[T, U](f: (T, T, T) => U): (List[T]) => U = (ts:List[T]) => ts match {
    case x::y::z::Nil => f(x, y, z)
    case _ => error("expected 3 args, but got: " + ts)
  }
}