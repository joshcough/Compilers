package L1Compiler

object L1AST {

  object L1{ def apply(main: Func): L1 = L1(main, Nil) }
  case class L1(main: Func, funs:List[Func]){
    def toCode = "(" + main.toCode(false) + "\n" + funs.map(_.toCode(true)).mkString("\n") + ")"
  }
  case class Func(name: LabelDeclaration, body: List[Instruction]){
    def toCode(printLabel:Boolean=true) = {
      if(printLabel) "(" + name.l.toCode + "\n" + body.map(_.toCode).mkString("\n") + ")"
      else "(" + body.map(_.toCode).mkString("\n") + ")"
    }
  }

  // TODO: this is not an entire instruction
  // because its the right hand side of an assignment
  case class Allocate(n:S, init:S) extends Instruction {
    def toCode: String = "(eax <- (allocate " + n.toCode + " " + init.toCode + "))"
  }
  // s:Instruction here because it could be an S, or a MemRead
  case class Assignment(x:X, s:Instruction) extends Instruction {
    def toCode: String = "(" + x.toCode + " <- " + s.toCode + ")"
  }
  case class Increment(x:X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " += " + s.toCode + ")"
  }
  case class Decrement(x:X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " -= " + s.toCode + ")"
  }
  case class Multiply(x:X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " *= " + s.toCode + ")"
  }
  case class LeftShift(x:X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " <<= " + s.toCode + ")"
  }
  case class RightShift(x:X, s:S) extends Instruction {
    def toCode: String = "(" + s.toCode + " >>= " + x.toCode + ")"
  }
  case class BitwiseAnd(x:X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " &= " + s.toCode + ")"
  }
  // TODO: this is not an entire instruction
  case class MemLoc(basePointer: X, offset: Num) extends Instruction {
    def toCode: String = "(mem " + basePointer.toCode + " " + offset.toCode + ")"
  }
  // TODO: this is not an entire instruction
  case class MemRead(loc: MemLoc) extends Instruction {
    def toCode: String = loc.toCode
  }
  case class MemWrite(loc: MemLoc, s:S) extends Instruction {
    def toCode: String = "(" + loc.toCode + " <- " + s.toCode + ")"
  }
  // TODO: this is not an entire instruction
  // because its the right hand side of an assignment
  case class Print(s:S) extends Instruction {
    def toCode: String = "(eax <- (print " + s.toCode + "))"
  }
  // TODO: check if interpreter allows (goto num) and (goto register)
  case class Goto(s:S) extends Instruction {
    def toCode: String = "(goto " + s.toCode + ")"
  }
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction {
    def toCode: String = "(cjump " + comp.toCode + " " + l1.toCode  + " " + l2.toCode + ")"
  }
  case class Call(s:S) extends Instruction {
    def toCode: String = "(call " + s.toCode + ")"
  }
  case object Return extends Instruction {
    def toCode: String = "(return)"
  }

  trait Instruction{
    def toCode: String
  }
  trait S extends Instruction
  trait X extends S
  case class Num(n: Int) extends S {
    def toCode: String = n.toString
  }
  case class Label(l: String) extends S {
    override def toString = "Label(\"" + l + "\")"
    def toCode: String = ":" + l
  }
  case class LabelDeclaration(l: Label) extends Instruction {
    def toCode: String = l.toCode
  }

  trait Register extends X {
    val name: String
    override def toString = name
    def toCode: String = name
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
  sealed abstract case class CXRegister(name: String) extends Register{
    def low8 = "%" + name(1) + "l"
  }
  object eax extends CXRegister("eax")
  object ecx extends CXRegister("ecx")
  object edx extends CXRegister("edx")
  object ebx extends CXRegister("ebx")

  // TODO: this is DEFINITELY not an instruction
  // its only part of two different instructions
  case class Comp(s1:S, op: CompOp, s2:S) extends Instruction {
    def toCode: String = s1.toCode + " " + op.op + " " + s2.toCode
  }
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
}