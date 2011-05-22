package L5Compiler

import L4Compiler.L4AST._

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
  class Prim(val keyword:Keyword) extends E{ def name = keyword.name }
  case class Num(n: Int) extends E
  class Biop(k:Keyword) extends Prim(k)
  class Pred(k:Keyword) extends Prim(k)
  case object Print extends Prim(print)
  case object NewArray extends Prim(newarr)
  case object ARef extends Prim(aref)
  case object ASet extends Prim(aset)
  case object ALen extends Prim(alen)
  case object Add extends Biop(add)
  case object Sub extends Biop(sub)
  case object Mult extends Biop(mult)
  case object LessThan extends Biop(lt)
  case object LessThanOrEqualTo extends Biop(lteq)
  case object EqualTo extends Biop(L4Compiler.L4AST.eq)
  case object IsNumber extends Pred(num_?)
  case object IsArray extends Pred(a_?)
}