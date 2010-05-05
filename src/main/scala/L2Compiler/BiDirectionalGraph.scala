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
  def colorOf(x:X): Color = {
//    println("data: " + data)
//    println("x: " + x)
    data.find(_._1._1 == x).get._1._2
  }
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

  // only exists to make the code shorter
  private def rep(i:Instruction): Instruction = replaceVarsWithRegisters(i)
  private def repX(x:X): X = x match {
    case v:Variable => colorOf(v).register
    case _ => x
  }
  private def repLoc(loc:MemLoc): MemLoc = MemLoc(repX(loc.basePointer),loc.offset)
  private def repComp(c:Comp): Comp = Comp(repX(c.x1), c.op, repX(c.x2))

  def replaceVarsWithRegisters(i:Instruction): Instruction = i match {
    case Allocate(n, init) => Allocate(repX(n), repX(init))
    case Assignment(x1, x2:X) => Assignment(repX(x1), repX(x2))
    case Assignment(x, i) => Assignment(repX(x), rep(i))
    case Increment(s1, s2) => Increment(repX(s1), repX(s2))
    case Decrement(s1, s2) => Decrement(repX(s1), repX(s2))
    case Multiply(s1, s2) => Multiply(repX(s1), repX(s2))
    case LeftShift(s1, s2) => LeftShift(repX(s1), repX(s2))
    case RightShift(s1, s2) => RightShift(repX(s1), repX(s2))
    case BitwiseAnd(s1, s2) => BitwiseAnd(repX(s1), repX(s2))
    case MemRead(loc) => MemRead(repLoc(loc))
    case MemWrite(loc, x) => MemWrite(repLoc(loc), repX(x))
    case Print(x) => Print(repX(x))
    case Goto(x) => Goto(repX(x))
    case CJump(comp, l1, l2) => CJump(repComp(comp), l1, l2)
    case Call(x) => Call(repX(x))
    case _ => i
  }
}