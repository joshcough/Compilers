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

  sealed trait AssignmentRHS { def toCode:String }

  case class Allocate(n:S, init:S) extends AssignmentRHS {
    def toCode: String = "(allocate " + n.toCode + " " + init.toCode + ")"
  }
  case class Assignment(r:Register, rhs:AssignmentRHS) extends Instruction {
    def toCode: String = "(" + r.toCode + " <- " + rhs.toCode + ")"
  }
  case class Increment(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + r.toCode + " += " + s.toCode + ")"
  }
  case class Decrement(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + r.toCode + " -= " + s.toCode + ")"
  }
  case class Multiply(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + r.toCode + " *= " + s.toCode + ")"
  }
  case class LeftShift(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + r.toCode + " <<= " + s.toCode + ")"
  }
  case class RightShift(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + s.toCode + " >>= " + r.toCode + ")"
  }
  case class BitwiseAnd(r:Register, s:S) extends Instruction {
    def toCode: String = "(" + r.toCode + " &= " + s.toCode + ")"
  }
  case class MemLoc(basePointer:Register, offset: Num) {
    def toCode: String = "(mem " + basePointer.toCode + " " + offset.toCode + ")"
  }
  case class MemRead(loc: MemLoc) extends AssignmentRHS {
    def toCode: String = loc.toCode
  }
  case class MemWrite(loc: MemLoc, s:S) extends Instruction {
    def toCode: String = "(" + loc.toCode + " <- " + s.toCode + ")"
  }
  case class Print(s:S) extends AssignmentRHS {
    def toCode: String = "(print " + s.toCode + ")"
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
  case class TailCall(s:S) extends Instruction {
    def toCode: String = "(call " + s.toCode + ")"
  }
  case object Return extends Instruction {
    def toCode: String = "(return)"
  }
  case class ArrayError(s1:S, s2:S) extends AssignmentRHS {
    def toCode: String = "(array-error " + s1.toCode + " " + s2.toCode + ")"
  }

  sealed trait Instruction{
    def toCode: String
  }
  sealed trait S extends AssignmentRHS
  case class Num(n: Int) extends S {
    def toCode: String = n.toString
  }
  case class Label(name: String) extends S {
    override def toString = "Label(\"" + name + "\")"
    def toCode: String = ":" + name
  }
  case class LabelDeclaration(l: Label) extends Instruction {
    def toCode: String = l.toCode
  }

  sealed trait Register extends S {
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
  case class Comp(s1:S, op: CompOp, s2:S) extends AssignmentRHS {
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