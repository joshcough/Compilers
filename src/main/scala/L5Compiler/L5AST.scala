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
  class Prim(val name:String) extends E
  case class Num(n: Int) extends E
  class Biop(override val name:String) extends Prim(name)
  class Pred(override val name:String) extends Prim(name)
  case object Print extends Prim("print")
  case object NewArray extends Prim("new-array")
  case object ARef extends Prim("aref")
  case object ASet extends Prim("aset")
  case object ALen extends Prim("alen")
  case object Add extends Biop("+")
  case object Sub extends Biop("-")
  case object Mult extends Biop("*")
  case object LessThan extends Biop("<")
  case object LessThanOrEqualTo extends Biop("<=")
  case object EqualTo extends Biop("=")
  case object IsNumber extends Pred("number?")
  case object IsArray extends Pred("a?")
}