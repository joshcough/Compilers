package L1Compiler

object L1AST {

  trait L1ASTNode
  object L1{ def apply(main: Func): L1 = L1(main, Nil) }
  case class L1(main: Func, funs:List[Func]) extends L1ASTNode
  case class Func(body: List[Instruction]) extends L1ASTNode
  case class MemLoc(basePointer:Register, offset: Num) extends L1ASTNode

  // the main instructions
  sealed trait Instruction extends L1ASTNode
  case class Assignment(r:Register, rhs:AssignmentRHS) extends Instruction
  case class Increment(r:Register, s:S) extends Instruction
  case class Decrement(r:Register, s:S) extends Instruction
  case class Multiply(r:Register, s:S) extends Instruction
  case class LeftShift(r:Register, s:S) extends Instruction
  case class RightShift(r:Register, s:S) extends Instruction
  case class BitwiseAnd(r:Register, s:S) extends Instruction
  case class MemWrite(loc: MemLoc, s:S) extends Instruction
  case class Goto(label:Label) extends Instruction
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction
  case class LabelDeclaration(l: Label) extends Instruction
  case class Call(s:S) extends Instruction
  case class TailCall(s:S) extends Instruction
  case object Return extends Instruction

  // assignment right hand sides
  sealed trait AssignmentRHS extends L1ASTNode
  case class Allocate(n:S, init:S) extends AssignmentRHS
  case class Print(s:S) extends AssignmentRHS
  case class ArrayError(s1:S, s2:S) extends AssignmentRHS
  case class MemRead(loc: MemLoc) extends AssignmentRHS

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
  sealed trait Register extends S {
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

object L1Printer {
  import L1AST._
  def toCode(a:L1ASTNode): String = a match {
    case L1(main, funcs) => (main :: funcs).map(toCode).mkString("(", "\n", ")")
    case f:Func => f.body.map(toCode).mkString("(", "\n", ")")
    case Allocate(n:S, init:S) => "(allocate " + toCode(n) + " " + toCode(init) + ")"
    case Assignment(r:Register, rhs:AssignmentRHS) => "(" + toCode(r) + " <- " + toCode(rhs) + ")"
    case Increment(r:Register, s:S)  => "(" + toCode(r) + " += " + toCode(s) + ")"
    case Decrement(r:Register, s:S)  => "(" + toCode(r) + " -= " + toCode(s) + ")"
    case Multiply(r:Register, s:S)   => "(" + toCode(r) + " *= " + toCode(s) + ")"
    case BitwiseAnd(r:Register, s:S) => "(" + toCode(r) + " &= " + toCode(s) + ")"
    case LeftShift(r:Register, s:S)  => "(" + toCode(r) + " <<= " + toCode(s) + ")"
    case RightShift(r:Register, s:S) => "(" + toCode(r) + " >>= " + toCode(s) + ")"
    case MemLoc(basePointer:Register, offset: Num) => "(mem " + toCode(basePointer) + " " + toCode(offset) + ")"
    case MemRead(loc: MemLoc) => toCode(loc)
    case MemWrite(loc: MemLoc, s:S) => "(" + toCode(loc) + " <- " + toCode(s) + ")"
    case Print(s:S) => "(print " + toCode(s) + ")"
    case Goto(s:S) => "(goto " + toCode(s) + ")"
    case CJump(comp:Comp, l1: Label, l2: Label) => "(cjump " + toCode(comp) + " " + toCode(l1)  + " " + toCode(l2) + ")"
    case Call(s:S) => "(call " + toCode(s) + ")"
    case TailCall(s:S) => "(tail-call " + toCode(s) + ")"
    case Return => "(return)"
    case ArrayError(s1:S, s2:S) => "(array-error " + toCode(s1) + " " + toCode(s2) + ")"
    case Comp(s1:S, op: CompOp, s2:S) => toCode(s1) + " " + op.op + " " + toCode(s2)
    case Num(n) => n.toString
    case Label(name: String) => ":" + name
    case LabelDeclaration(l: Label) => toCode(l)
    case r:Register => r.name
  }
}