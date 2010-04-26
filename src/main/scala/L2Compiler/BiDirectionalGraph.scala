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


import java.awt.Color
import java.awt.Color._
import L2AST._

object RegisterColorGraph{
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
}