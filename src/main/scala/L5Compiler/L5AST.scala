package L5Compiler

object L5AST {
  sealed trait E
  case class Lambda(args:List[Variable], body: E) extends E
  case class Variable(name: String) extends E{ override def toString = name }
  case class Let(v:Variable, e:E, body:E) extends E
  case class LetRec(v:Variable, e:E, body:E) extends E
  case class IfStatement(e:E, truePath:E, falsePath:E) extends E
  case class NewTuple(vs:List[E]) extends E
  case class Begin(e1:E, e2:E) extends E
  case class App(f:E, args:List[E]) extends E
  trait Prim extends E
  case class Num(n: Int) extends E
  trait Biop extends Prim
  trait Pred extends Prim
  case object Print extends Prim
  case object NewArray extends Prim
  case object ARef extends Prim
  case object ASet extends Prim
  case object ALen extends Prim
  case object Add extends Biop
  case object Sub extends Biop
  case object Mult extends Biop
  case object LessThan extends Biop
  case object LessThanOrEqualTo extends Biop
  case object EqualTo extends Biop
  case object IsNumber extends Pred
  case object IsArray extends Pred
}