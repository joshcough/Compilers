package L2Compiler

object L2AST {

  trait L2ASTNode

  object L2{ def apply(main: Func): L2 = L2(main, Nil) }
  case class L2(main: Func, funs:List[Func]) extends L2ASTNode
  case class Func(body: List[Instruction]) extends L2ASTNode
  case class MemLoc(basePointer:X, offset: Num) extends L2ASTNode
  sealed trait X extends S with L2ASTNode // the S here is kinda funny, but leaving it.


  // the main instructions
  sealed trait Instruction extends L2ASTNode
  case class Assignment(lhs:X, rhs:AssignmentRHS) extends Instruction
  case class Increment (x:X, s:S) extends Instruction
  case class Decrement (x:X, s:S) extends Instruction
  case class Multiply  (x:X, s:S) extends Instruction
  case class LeftShift (x:X, s:S) extends Instruction
  case class RightShift(x:X, s:S) extends Instruction
  case class BitwiseAnd(x:X, s:S) extends Instruction
  case class MemWrite(loc: MemLoc, s:S) extends Instruction
  case class Goto(label:Label) extends Instruction
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction
  case class LabelDeclaration(l: Label) extends Instruction
  case class Call(s:S) extends Instruction
  case class TailCall(s:S) extends Instruction
  case object Return extends Instruction

  // assignment right hand sides
  sealed trait AssignmentRHS extends L2ASTNode
  case class Allocate(n:S, init:S) extends AssignmentRHS
  case class Print(s:S) extends AssignmentRHS
  case class ArrayError(s1:S, s2:S) extends AssignmentRHS
  case class MemRead(loc: MemLoc) extends AssignmentRHS
  case class Variable(name: String) extends S with X {
    override def toString = "Variable(\"" + name + "\")"
  }

  // comparisons
  case class Comp(s1:S, op: CompOp, s2:S) extends AssignmentRHS
  sealed abstract case class CompOp(op: String){
    def apply(x:Int, y:Int): Boolean
  }
  object LessThan extends CompOp("<"){
    override def toString = "LessThan"
    def apply(x:Int, y:Int) = x < y
  }
  object LessThanOrEqualTo extends CompOp("<="){
    override def toString = "LessThanOrEqualTo"
    def apply(x:Int, y:Int) = x <= y
  }
  object EqualTo extends CompOp("="){
    override def toString = "EqualTo"
    def apply(x:Int, y:Int) = x == y
  }

  // s's (registers, numbers, labels)
  trait S extends AssignmentRHS
  case class Num(n: Int) extends S
  case class Label(name: String) extends S {
    override def toString = "Label(\"" + name + "\")"
  }

  // registers
  sealed trait Register extends S with X{
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