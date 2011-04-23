package L3Compiler

object L3AST {

  trait L3ASTNode
  //p ::= (e (l (x ...) e) ...)
  object L3{ def apply(main: E): L3 = L3(main, Nil) }
  case class L3(main: E, funs:List[Func]) extends L3ASTNode
  //(l (x ...) e)
  type Argument = String
  case class Func(l:Label, args:List[Argument], body: E) extends L3ASTNode

  trait E extends L3ASTNode
  case class Let(x:X, d:D, body:E) extends E
  case class IfStatement(v:V, truePath:E, falsePath:E) extends E
  trait D extends E

  trait Biop extends D { val left:V; val right: V }
  case class Add(left:V, right:V) extends Biop
  case class Sub(left:V, right:V) extends Biop
  case class Mult(left:V, right:V) extends Biop
  case class LessThan(left:V, right:V) extends Biop
  case class LessThanOrEqualTo(left:V, right:V) extends Biop
  case class EqTo(left:V, right:V) extends Biop

  trait Pred extends D
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
  trait V extends D

  sealed trait X extends V with L3ASTNode with Ordered[X]{
    val name: String
    def compare(that:X) = this.name.compare(that.name)
  }

  case class Variable(name: String) extends X

  case class Num(n: Int) extends V
  case class Label(name: String) extends V {
    override def toString = "Label(\"" + name + "\")"
  }

  // registers
  sealed trait Register extends V with X {
    val name: String
    override def toString = name
  }
  object XRegister {
    def apply(s: Symbol): Option[XRegister] = s match {
      case 'esi => Some(esi)
      case 'edi => Some(edi)
      case 'ebp => Some(ebp)
      case 'esp => Some(esp)
      case _ => None
    }
  }
  sealed abstract case class XRegister(name: String) extends Register
  object esi extends XRegister("esi")
  object edi extends XRegister("edi")
  object ebp extends XRegister("ebp")
  object esp extends XRegister("esp")
  object CXRegister {
    def apply(s: Symbol): Option[CXRegister] = s match {
      case 'eax => Some(eax)
      case 'ecx => Some(ecx)
      case 'edx => Some(edx)
      case 'ebx => Some(ebx)
      case _ => None
    }
  }
  sealed abstract case class CXRegister(name: String) extends Register {
    def low8 = "%" + name(1) + "l"
  }
  object eax extends CXRegister("eax")
  object ecx extends CXRegister("ecx")
  object edx extends CXRegister("edx")
  object ebx extends CXRegister("ebx")
}