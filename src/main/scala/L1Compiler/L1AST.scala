package L1Compiler

object L1AST {

  object L1{ def apply(main: L1Function): L1 = L1(main, Nil) }
  case class L1(main: L1Function, funs:List[L1Function])

  trait Instruction
  trait S extends Instruction
  case class Num(n: Int) extends S
  case class Label(l: String) extends S
  case class LabelDeclaration(l: Label) extends Instruction
  abstract class Register(val name: String) extends S
  object XRegister {
    def apply(s: Symbol): Option[XRegister] = s match {
      case 'esi => Some(esi)
      case 'edi => Some(edi)
      case 'ebp => Some(ebp)
      case 'esp => Some(esp)
      case _ => None
    }
  }
  sealed abstract case class XRegister(x: String) extends Register(x)
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
  sealed abstract case class CXRegister(cx: String) extends Register(cx)
  object eax extends CXRegister("eax")
  object ecx extends CXRegister("ecx")
  object edx extends CXRegister("edx")
  object ebx extends CXRegister("ebx")

  case class Comp(s1: S, op: CompOp, s2: S) extends Instruction
  sealed abstract case class CompOp(op: String)
  object LessThan extends CompOp("<")
  object LessThanOrEqualTo extends CompOp("<=")
  object EqualTo extends CompOp("=")
  case class RegisterAssignment(r: Register, s: Instruction) extends Instruction
  case class MemLoc(basePointer: Register, offset: Num) extends Instruction
  case class MemRead(loc: MemLoc) extends Instruction
  case class MemWrite(loc: MemLoc, e: S) extends Instruction
  case class Print(e: S) extends Instruction
  case class Allocate(s:S, init: Num) extends Instruction
  case class Increment(x: Register, s: S) extends Instruction
  case class Decrement(x: Register, s: S) extends Instruction
  case class Multiply(x: Register, s: S) extends Instruction
  // TODO: check if interpreter allows (goto num) and (goto register)
  case class Goto(s: S) extends Instruction
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction
  case class Call(s:S) extends Instruction
  case object Return extends Instruction

  case class L1Function(name: LabelDeclaration, body: List[Instruction])

}