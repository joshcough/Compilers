package L4Compiler

object L4AST extends FunctionLifters{

  sealed trait L4ASTNode
  //p::= (e (l (x ...) e) ...)
  object L4{ def apply(main: E): L4 = L4(main, Nil) }
  case class L4(main: E, funs:List[Func]) extends L4ASTNode
  //(l (x ...) e)
  case class Func(label:Label, args:List[Variable], body: E) extends L4ASTNode

  sealed trait E extends L4ASTNode

  trait EN extends E { def first: E; def rest: List[E]; def rebuild: (List[E]) => E }
  sealed trait E1 extends EN{
    def e: E; def first = e; def rest = Nil; def rebuild1: E => E; def rebuild = lift1(rebuild1)
  }
  sealed trait E2 extends EN {
    def e1: E; def e2: E; def first = e1; def rest = List(e2)
    def rebuild2: (E, E) => E; def rebuild = lift2(rebuild2)
  }
  sealed trait E3 extends EN {
    def e1: E; def e2: E; def e3: E; def first = e1; def rest = List(e2, e3);
    def rebuild3: (E, E, E) => E; def rebuild = lift3(rebuild3)
  }
  //(let ((x e)) e)
  case class Let(v:Variable, e:E, body:E) extends E
  case class IfStatement(e:E, truePath:E, falsePath:E) extends E
  // TODO: find/fill for begin
  case class Begin(e1:E, e2:E) extends E2 {
    val rebuild2 = Begin.apply _
  }
  case class FunCall(f:E, args:List[E]) extends EN {
    def first = f; def rest = args; def rebuild = (es:List[E]) => new FunCall(es.head, es.tail)
  }

  trait Biop extends E2 { val left:E; val right:E; val e1 = left; val e2 = right }
  case class Add(left:E, right:E) extends Biop{ val rebuild2 = Add.apply _ }
  case class Sub(left:E, right:E) extends Biop{ val rebuild2 = Sub.apply _ }
  case class Mult(left:E, right:E) extends Biop{ val rebuild2 = Mult.apply _ }
  case class LessThan(left:E, right:E) extends Biop{ val rebuild2 = LessThan.apply _ }
  case class LessThanOrEqualTo(left:E, right:E) extends Biop{ val rebuild2 = LessThanOrEqualTo.apply _ }
  case class EqualTo(left:E, right:E) extends Biop{ val rebuild2 = EqualTo.apply _ }

  sealed trait Pred extends E1
  case class IsNumber(e:E) extends Pred{ val rebuild1 = IsNumber.apply _ }
  case class IsArray(e:E) extends Pred{ val rebuild1 = IsArray.apply _ }

  case class NewArray(size:E, init:E) extends E2 {
    val e1 = size; val e2 = init; val rebuild2 = NewArray.apply _
  }
  // TODO: takes N args
  case class NewTuple(vs:List[E]) extends EN{
    def first = vs.head; def rest = vs.tail; def rebuild = NewTuple.apply _
  }
  case class ARef(arr:E, loc:E) extends E2 {
    val e1 = arr; val e2 = loc; val rebuild2 = ARef.apply _
  }
  // TODO: takes 3 args
  case class ASet(arr:E, loc:E, newVal: E) extends E3{
    val e1 = arr; val e2 = loc; val e3 = newVal; val rebuild3 = ASet.apply _
  }
  case class ALen(arr:E)  extends E1{ val e = arr; val rebuild1 = ALen.apply _ }
  case class Print(e:E) extends E1{ val rebuild1 = Print.apply _ }
  case class MakeClosure(l:Label, e:E) extends E1{ val rebuild1 = (e:E) => MakeClosure.apply(l, e) }
  case class ClosureProc(e:E) extends E1{ val rebuild1 = ClosureProc.apply _ }
  case class ClosureVars(e:E) extends E1{ val rebuild1 = ClosureVars.apply _ }

  trait V extends E

  case class Variable(name: String) extends V with Ordered[Variable] {
    def compare(that:Variable) = this.name.compare(that.name)
  }

  case class Num(n: Int) extends V {
    def *(i:Int) = Num(n*i)
    def +(i:Int) = Num(n+i)
  }
  case class Label(name: String) extends V {
    override def toString = "Label(\"" + name + "\")"
  }
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