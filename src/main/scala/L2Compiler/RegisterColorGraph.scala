package L2Compiler

import L2AST._

// TODO....this thing is probably mostly going away.
// Mostly replaced by InterferenceGraph.
// some of this seems useful and salvagable though.
object RegisterColorGraph{
  case class Color(name:String, register: Register)
  object RED extends Color("Red", eax)
  object GREEN extends Color("Green", edx)
  object BLUE extends Color("Blue", ecx)
  object YELLOW extends Color("Yellow", ebx)
  object CYAN extends Color("Cyan", edi)
  object MAGENTA extends Color("Magenta", esi)
  object GRAY extends Color("Gray", ebp) // EBP HACK! haha?
  class ColoredNode(val x:X, val color:Color) extends Tuple2(x, color)
  val colors = List(RED,GREEN,BLUE,YELLOW,CYAN,MAGENTA)
  def base: RegisterColorGraph = {
    val eaxRed     = new ColoredNode(eax, RED)
    val edxGreen   = new ColoredNode(edx, GREEN)
    val ecxBlue    = new ColoredNode(ecx, BLUE)
    val ebxYellow  = new ColoredNode(ebx, YELLOW)
    val ediCyan    = new ColoredNode(edi, CYAN)
    val esiMagenta = new ColoredNode(esi, MAGENTA)
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
  def colorOf(x:X): Color = {
    data.find(_._1.x == x).get._1.color
  }
  // inserts the new item as GRAY
  def connect(itemNotInGraph:X, itemInGraph:X): RegisterColorGraph = {
    RegisterColorGraph(data + (new ColoredNode(itemInGraph, GRAY) -> new ColoredNode(itemInGraph, colorOf(itemInGraph))))
  }
  def member_?(x:X) = data.find(n => n._1.x == x).isDefined
  def connected_?(x1:X, x2:X) = data.find(n => n._1.x == x1 && n._2.x == x2).isDefined
  def addInterference(connections:Set[(X,X)]): RegisterColorGraph = connections.toList match {
    case Nil => this
    case (x1,x2)::xs => {
      val x1Color = if(member_?(x1)) colorOf(x1) else GRAY
      val x2Color = if(member_?(x2)) colorOf(x2) else GRAY
      RegisterColorGraph(data + (new ColoredNode(x1, x1Color) -> new ColoredNode(x2, x2Color))).addInterference(xs.toSet)
    }
  }
  def color: Option[RegisterColorGraph] = {
    def isGray(node:ColoredNode) = node._2 == GRAY
    // if there are no GRAY nodes, then the graph was colored ok.
    data.find{ case (t1,t2) => isGray(t1) || isGray(t2) } match {
      case None => Some(this) // Colored!
      case Some((cn1:ColoredNode, cn2:ColoredNode)) => {
        val greyNode = if(isGray(cn1)) cn1 else cn2
        val neighborColors = data.neigborsOf(greyNode).map(_.color)
        val remainingColors = colors filterNot neighborColors.contains
        remainingColors match {
          case theFreeColor::rest =>
            RegisterColorGraph(data.replace(greyNode, new ColoredNode(greyNode._1, theFreeColor))).color
          case Nil => None
        }
      }
    }
  }

  def replaceVarsWithRegisters(f:Func): Func = Func(f.body.map(replaceVarsWithRegisters))

  private def getRegister(x:X): Register = x match {
    case v:Variable => colorOf(v).register
    case r:Register => r
  }

  def replaceVarsWithRegisters(i:Instruction): Instruction = i match {
    case Assignment(x1, x2) => Assignment(getRegister(x1), replaceVarsWithRegisters(x2))
    case Increment(s1, s2) => Increment(getRegister(s1), replaceVarsWithRegisters(s2))
    case Decrement(s1, s2) => Decrement(getRegister(s1), replaceVarsWithRegisters(s2))
    case Multiply(s1, s2) => Multiply(getRegister(s1), replaceVarsWithRegisters(s2))
    case BitwiseAnd(s1, s2) => BitwiseAnd(getRegister(s1), replaceVarsWithRegisters(s2))
    case LeftShift(s1, s2) => LeftShift(getRegister(s1), replaceVarsWithRegisters(s2))
    case RightShift(s1, s2) => RightShift(getRegister(s1), replaceVarsWithRegisters(s2))
    case MemWrite(loc, x) => MemWrite(replaceVarsWithRegisters(loc), replaceVarsWithRegisters(x))
    case g:Goto => g
    case CJump(comp, l1, l2) => CJump(replaceVarsWithRegisters(comp), l1, l2)
    case Call(x) => Call(replaceVarsWithRegisters(x))
    case TailCall(x) => Call(replaceVarsWithRegisters(x))
    case ld:LabelDeclaration => ld
    case Return => Return
  }

  def replaceVarsWithRegisters(s:S): S = s match {
    case r:Register => r
    case v:Variable => getRegister(v)
    case n:Num => n
    case l:Label => l
  }

  def replaceVarsWithRegisters(rhs:AssignmentRHS):AssignmentRHS = rhs match {
    case MemRead(loc) => MemRead(replaceVarsWithRegisters(loc))
    case Print(s) => Print(replaceVarsWithRegisters(s))
    case Allocate(n, init) => Allocate(replaceVarsWithRegisters(n), replaceVarsWithRegisters(init))
    case ArrayError(a, n) => Allocate(replaceVarsWithRegisters(a), replaceVarsWithRegisters(n))
    case c:Comp => replaceVarsWithRegisters(c)
    case s:S => replaceVarsWithRegisters(s)
  }

  private def replaceVarsWithRegisters(loc:MemLoc): MemLoc =
    MemLoc(getRegister(loc.basePointer),loc.offset)
  private def replaceVarsWithRegisters(c:Comp): Comp =
    Comp(replaceVarsWithRegisters(c.s1), c.op, replaceVarsWithRegisters(c.s2))
}