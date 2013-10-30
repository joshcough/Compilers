package L3Compiler

object L3AST {

  sealed trait L3ASTNode
  //p ::= (e (l (x ...) e) ...)
  object L3{ def apply(main: E): L3 = L3(main, Nil) }
  case class L3(main: E, funs:List[Func]) extends L3ASTNode
  //(l (x ...) e)
  case class Func(label:Label, args:List[Variable], body: E) extends L3ASTNode

  sealed trait E extends L3ASTNode
  case class Let(v:Variable, d:D, body:E) extends E
  case class IfStatement(v:V, truePath:E, falsePath:E) extends E
  sealed trait D extends E

  sealed trait Biop extends D { val left:V; val right: V }
  case class Add(left:V, right:V) extends Biop
  case class Sub(left:V, right:V) extends Biop
  case class Mult(left:V, right:V) extends Biop
  case class LessThan(left:V, right:V) extends Biop
  case class LessThanOrEqualTo(left:V, right:V) extends Biop
  case class EqualTo(left:V, right:V) extends Biop

  sealed trait Pred extends D
  case class IsNumber(v:V) extends Pred
  case class IsArray(v:V) extends Pred

  case class FunCall(v:V, args:List[V]) extends D

  case class NewArray(size:V, init:V) extends D
  case class NewTuple(vs:List[V]) extends D
  case class ARef(arr:V, loc:V) extends D
  case class ASet(arr:V, loc:V, newVal: V) extends D
  case class ALen(arr:V) extends D

  case class Print(v:V) extends D
  case class MakeClosure(l:Label, v:V) extends D
  case class ClosureProc(v:V) extends D
  case class ClosureVars(v:V) extends D

  // v's (registers, numbers, labels)
  sealed trait V extends D

  case class Variable(name: String) extends V

  case class Num(n: Int) extends V {
    def *(i:Int) = Num(n*i)
    def +(i:Int) = Num(n+i)
  }
  case class Label(name: String) extends V {
    override def toString = "Label(\"" + name + "\")"
  }

  def isV(d:D) = d.isInstanceOf[V]
}