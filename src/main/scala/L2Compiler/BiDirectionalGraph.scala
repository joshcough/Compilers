package L2Compiler

object BiDirectionalGraph{
  def apply[T](conns:(T,T)*): BiDirectionalGraph[T] = {
    new BiDirectionalGraph[T](Set()) ++ conns.toList
  }
}

case class BiDirectionalGraph[T](connections:Set[(T,T)]){
  def ++(cons: List[(T,T)]): BiDirectionalGraph[T] = cons match {
    case c::cs => (this + c) ++ cs
    case _ => this
  }
  def + (con: (T,T)) = new BiDirectionalGraph(connections + con + (con._2 -> con._1))
  def contains(con:(T,T)) = connections.contains(con) || connections.contains(con._2 -> con._1)
  def map[B](f: ((T,T)) => (B,B)) = new BiDirectionalGraph(connections.map(f))
  def filter(p: ((T,T)) => Boolean) = new BiDirectionalGraph(connections.filter(p))
  def find(p: ((T,T)) => Boolean) = connections.find(p)
  def neigborsOf(t:T) = connections.filter(_._1 == t).map(_._2)
  def replace(t1:T, t2:T) = new BiDirectionalGraph(connections.map{
    case (x1,x2) => if(t1 == x1) (t2, x2) else if (t1 == x2) (x1,t2) else (x1,x2)
  })
}

import L2AST._
import L1Compiler.L1AST._

object RegisterColorGraph{
  case class Color(name:String, register: Register)
  object RED extends Color("Red", eax)
  object GREEN extends Color("Green", edx)
  object BLUE extends Color("Blue", ecx)
  object YELLOW extends Color("Yellow", ebx)
  object CYAN extends Color("Cyan", edi)
  object MAGENTA extends Color("Magenta", esi)
  object GRAY extends Color("Gray", ebp) // EBP HACK! haha.
  type ColoredNode = (X, Color)
  val colors = List(RED,GREEN,BLUE,YELLOW,CYAN,MAGENTA)
  def base: RegisterColorGraph = {
    val eaxRed: ColoredNode = (eax -> RED)
    val edxGreen: ColoredNode = (edx -> GREEN)
    val ecxBlue: ColoredNode = (ecx -> BLUE)
    val ebxYellow: ColoredNode = (ebx -> YELLOW)
    val ediCyan: ColoredNode = (edi -> CYAN)
    val esiMagenta: ColoredNode = (esi -> MAGENTA)
    RegisterColorGraph(new BiDirectionalGraph(Set(
      eaxRed -> edxGreen, eaxRed -> ecxBlue, eaxRed -> ebxYellow, eaxRed -> ediCyan, eaxRed -> esiMagenta,
      edxGreen -> ecxBlue, edxGreen -> ebxYellow, edxGreen -> ediCyan, edxGreen -> esiMagenta,
      ecxBlue -> ebxYellow, ecxBlue -> ediCyan, ecxBlue -> esiMagenta,
      ebxYellow -> ediCyan, ebxYellow -> esiMagenta,
      ediCyan -> esiMagenta)))
  }
}

import RegisterColorGraph._

case class RegisterColorGraph(data:BiDirectionalGraph[ColoredNode]){
  def colorOf(x:X): Color = data.find(_._1._1 == x).get._1._2
  // inserts the new item as GRAY
  def connect(itemNotInGraph:X, itemInGraph:X): RegisterColorGraph = {
    RegisterColorGraph(data + ((itemInGraph -> GRAY) -> (itemInGraph -> colorOf(itemInGraph))))
  }
  def member_?(x:X) = data.find(n => n._1._1 == x).isDefined
  def connected_?(x1:X, x2:X) = data.find(n => n._1._1 == x1 && n._2._1 == x2).isDefined
  def addInterference(connections:Set[(X,X)]): RegisterColorGraph = connections.toList match {
    case Nil => this
    case (x1,x2)::xs => {
      val x1Color = if(member_?(x1)) colorOf(x1) else GRAY
      val x2Color = if(member_?(x2)) colorOf(x2) else GRAY
      RegisterColorGraph(data + ((x1 -> x1Color -> (x2 -> x2Color)))).addInterference(xs.toSet)
    }
  }
  def color: Option[RegisterColorGraph] = {
    def isGray(node:ColoredNode) = node._2 == GRAY
    // if there are no GRAY nodes, then the graph was colored ok.
    data.find{ case (t1,t2) => isGray(t1) || isGray(t2) } match {
      case None => Some(this) // Colored!
      case Some((cn1:ColoredNode, cn2:ColoredNode)) => {
        val greyNode = if(isGray(cn1)) cn1 else cn2
        val neighborColors = data.neigborsOf(greyNode).map(_._2)
        val remainingColors = colors filterNot neighborColors.contains
        remainingColors match {
          case theFreeColor::rest =>
            RegisterColorGraph(data.replace(greyNode, (greyNode._1, theFreeColor))).color
          case Nil => None
        }
      }
    }
  }
  def replaceVarsWithRegisters(f:Func): Func = {
    Func(f.name, f.body.map(replaceVarsWithRegisters))
  }

  def replaceVarsWithRegisters(i:Instruction): Instruction = {
    i
  }
}

/**
 *   case class Allocate(n:S, init: S) extends Instruction {
    def toCode: String = "(eax <- (allocate " + n.toCode + " " + init.toCode + "))"
  }
  case class Assignment(x: X, s: Instruction) extends Instruction {
    def toCode: String = "(" + x.toCode + " <- " + s.toCode + ")"
  }
  case class Increment(x: X, s: S) extends Instruction {
    def toCode: String = "(" + x.toCode + " += " + s.toCode + ")"
  }
  case class Decrement(x: X, s: S) extends Instruction {
    def toCode: String = "(" + x.toCode + " -= " + s.toCode + ")"
  }
  case class Multiply(x: X, s: S) extends Instruction {
    def toCode: String = "(" + x.toCode + " *= " + s.toCode + ")"
  }
  case class LeftShift(x: X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " <<= " + s.toCode + ")"
  }
  case class RightShift(x: X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " >>= " + s.toCode + ")"
  }
  case class BitwiseAnd(x: X, s:S) extends Instruction {
    def toCode: String = "(" + x.toCode + " &= " + s.toCode + ")"
  }
  case class MemLoc(basePointer: X, offset: Num) extends Instruction {
    def toCode: String = "(mem " + basePointer.toCode + " " + offset.toCode + ")"
  }
  case class MemRead(loc: MemLoc) extends Instruction {
    def toCode: String = loc.toCode
  }
  case class MemWrite(loc: MemLoc, e: S) extends Instruction {
    def toCode: String = "(" + loc.toCode + " <- " + e.toCode + ")"
  }
  case class Print(e: S) extends Instruction {
    def toCode: String = "(print " + e.toCode + ")"
  }
  // TODO: check if interpreter allows (goto num) and (goto register)
  case class Goto(s: S) extends Instruction {
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
}

trait Instructions {
  trait Instruction{
    def toCode: String
  }
  trait S extends Instruction
  case class Num(n: Int) extends S {
    def toCode: String = n.toString
  }
  case class Label(l: String) extends S {
    def toCode: String = ":" + l
  }
  case class LabelDeclaration(l: Label) extends Instruction {
    def toCode: String = l.toCode
  }
  trait X extends S
}

trait Registers extends Instructions {
  abstract class Register(val name: String) extends X {
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

trait Comps extends Instructions{
  case class Comp(s1: S, op: CompOp, s2: S) extends Instruction {
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
 */
