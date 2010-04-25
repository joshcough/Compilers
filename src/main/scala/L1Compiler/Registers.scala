package L1Compiler

trait Instructions {
  trait Instruction
  trait S extends Instruction
  case class Num(n: Int) extends S
  case class Label(l: String) extends S
  case class LabelDeclaration(l: Label) extends Instruction
  trait X extends S  
}

trait Registers extends Instructions {
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
}