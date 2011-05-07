package L4Compiler

object L4AST {

  sealed trait L4ASTNode
  //p::= (e (l (x ...) e) ...)
  object L4{ def apply(main: E): L4 = L4(main, Nil) }
  case class L4(main: E, funs:List[Func]) extends L4ASTNode
  //(l (x ...) e)
  case class Func(label:Label, args:List[Variable], body: E) extends L4ASTNode

  sealed trait E extends L4ASTNode
  //(let ((x e)) e)
  case class Let(v:Variable, e:E, body:E) extends E
  case class IfStatement(e:E, truePath:E, falsePath:E) extends E

  trait Biop extends E{ val left:E; val right:E; val rebuild:(E, E) => Biop }
  case class Add(left:E, right:E) extends Biop{ val rebuild = Add.apply _ }
  case class Sub(left:E, right:E) extends Biop{ val rebuild = Sub.apply _ }
  case class Mult(left:E, right:E) extends Biop{ val rebuild = Mult.apply _ }
  case class LessThan(left:E, right:E) extends Biop{ val rebuild = LessThan.apply _ }
  case class LessThanOrEqualTo(left:E, right:E) extends Biop{ val rebuild = LessThanOrEqualTo.apply _ }
  case class EqualTo(left:E, right:E) extends Biop{ val rebuild = EqualTo.apply _ }

  case class Begin(l:E, r:E) extends E

  sealed trait Pred extends E
  case class IsNumber(e:E) extends Pred
  case class IsArray(e:E) extends Pred

  case class FunCall(e:E, args:List[E]) extends E

  case class NewArray(size:E, init:E) extends E
  case class NewTuple(vs:List[E]) extends E
  case class ARef(arr:E, loc:E) extends E
  case class ASet(arr:E, loc:E, newVal: E) extends E
  case class ALen(arr:E) extends E

  case class Print(e:E) extends E
  case class MakeClosure(l:Label, e:E) extends E
  case class ClosureProc(e:E) extends E
  case class ClosureVars(e:E) extends E

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