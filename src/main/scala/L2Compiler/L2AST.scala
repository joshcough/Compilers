package L2Compiler

object L2AST {

  object L2{ def apply(main: L2Function): L2 = L2(main, Nil) }
  case class L2(main: L2Function, funs:List[L2Function])

  trait Instruction
  trait S extends Instruction
  case class Num(n: Int) extends S
  case class Label(l: String) extends S
  case class LabelDeclaration(l: Label) extends Instruction
  trait X extends S
  case class Variable(val name: String) extends X
  abstract class Register(val name: String) extends X
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
  sealed abstract case class CXRegister(cx: String) extends Register(cx){
    def low8 = "%" + cx(1) + "l"
  }
  object eax extends CXRegister("eax")
  object ecx extends CXRegister("ecx")
  object edx extends CXRegister("edx")
  object ebx extends CXRegister("ebx")

  case class Comp(s1: S, op: CompOp, s2: S) extends Instruction
  sealed abstract case class CompOp(op: String){
    def apply(x:Int, y:Int): Boolean
  }
  object LessThan extends CompOp("<"){
    def apply(x:Int, y:Int) = x < y
  }
  object LessThanOrEqualTo extends CompOp("<="){
    def apply(x:Int, y:Int) = x <= y
  }
  object EqualTo extends CompOp("="){
    def apply(x:Int, y:Int) = x == y
  }

  case class Allocate(n:S, init: S) extends Instruction
  case class Assignment(x: X, s: Instruction) extends Instruction
  case class Increment(x: X, s: S) extends Instruction
  case class Decrement(x: X, s: S) extends Instruction
  case class Multiply(x: X, s: S) extends Instruction
  case class LeftShift(x: X, s:S) extends Instruction
  case class RightShift(x: X, s:S) extends Instruction
  case class BitwiseAnd(x: X, s:S) extends Instruction
  case class MemLoc(basePointer: X, offset: Num) extends Instruction
  case class MemRead(loc: MemLoc) extends Instruction
  case class MemWrite(loc: MemLoc, e: S) extends Instruction
  case class Print(e: S) extends Instruction
  // TODO: check if interpreter allows (goto num) and (goto register)
  case class Goto(s: S) extends Instruction
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction
  case class Call(s:S) extends Instruction
  case object Return extends Instruction
  case class L2Function(name: LabelDeclaration, body: List[Instruction])
}
