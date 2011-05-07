package L4Compiler

object L4AST {

  sealed trait L4ASTNode
  //p::= (e (l (x ...) e) ...)
  object L4{ def apply(main: E): L4 = L4(main, Nil) }
  case class L4(main: E, funs:List[Func]) extends L4ASTNode
  //(l (x ...) e)
  case class Func(label:Label, args:List[Variable], body: E) extends L4ASTNode

  sealed trait E extends L4ASTNode
  sealed trait E1 extends E{ val e: E; val rebuild: E => E }
  sealed trait E2 extends E{ val e1: E; val e2: E; val rebuild: (E, E) => E }

  //(let ((x e)) e)
  case class Let(v:Variable, e:E, body:E) extends E
  case class IfStatement(e:E, truePath:E, falsePath:E) extends E
  // TODO: find/fill for begin
  case class Begin(l:E, r:E) extends E
  case class FunCall(f:E, args:List[E]) extends E {
    def rebuild = (vs:List[V]) => new FunCall(vs.head, vs.tail)
  }

  trait Biop extends E2 { val left:E; val right:E; val e1 = left; val e2 = right }
  case class Add(left:E, right:E) extends Biop{ val rebuild = Add.apply _ }
  case class Sub(left:E, right:E) extends Biop{ val rebuild = Sub.apply _ }
  case class Mult(left:E, right:E) extends Biop{ val rebuild = Mult.apply _ }
  case class LessThan(left:E, right:E) extends Biop{ val rebuild = LessThan.apply _ }
  case class LessThanOrEqualTo(left:E, right:E) extends Biop{ val rebuild = LessThanOrEqualTo.apply _ }
  case class EqualTo(left:E, right:E) extends Biop{ val rebuild = EqualTo.apply _ }

  sealed trait Pred extends E1{ val e: E }
  case class IsNumber(e:E) extends Pred{ val rebuild = IsNumber.apply _ }
  case class IsArray(e:E) extends Pred{ val rebuild = IsArray.apply _ }

  case class NewArray(size:E, init:E) extends E2{
    val e1 = size; val e2 = init; val rebuild = NewArray.apply _
  }
  // TODO: takes N args
  case class NewTuple(vs:List[E]) extends E
  case class ARef(arr:E, loc:E) extends E2{
    val e1 = arr; val e2 = loc; val rebuild = ARef.apply _
  }
  // TODO: takes 3 args
  case class ASet(arr:E, loc:E, newVal: E) extends E
  case class ALen(arr:E)  extends E1{  val e = arr; val rebuild = ALen.apply _ }
  case class Print(e:E) extends E1{ val rebuild = Print.apply _ }
  case class MakeClosure(l:Label, e:E) extends E1{ val rebuild = (e:E) => MakeClosure.apply(l, e) }
  case class ClosureProc(e:E) extends E1{ val rebuild = ClosureProc.apply _ }
  case class ClosureVars(e:E) extends E1{ val rebuild = ClosureVars.apply _ }

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