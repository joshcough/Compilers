package L2Compiler

import L2AST._

object InterferenceGraph {
  def base: InterferenceGraph = {
    InterferenceGraph(new BiDirectionalGraph(Set(
      eax -> ebx, eax -> ecx, eax -> edi, eax -> edx, eax -> esi,
      ebx -> ecx, ebx -> edi, ebx -> edx, ebx -> esi,
      ecx -> edi, ecx -> edx, ecx -> esi,
      edi -> edx, edi -> esi,
      edx -> esi)))
  }
}

case class InterferenceGraph(data:BiDirectionalGraph[X]){
  def sortedMembers = data.members.toList.sortWith(compare)

  // TODO: this could all get cleaned up.
  def addInterference(connections:(X,X)*): InterferenceGraph = addInterference(connections.toSet)
  def addInterference(connections:Set[(X,X)]): InterferenceGraph = connections.toList match {
    case Nil => this
    case (x1,x2)::xs => InterferenceGraph(data + (x1 -> x2)).addInterference(xs.toSet)
  }

  def compare(a:X, b:X) = L2Printer.toCode(a) < L2Printer.toCode(b)
  def sortedNeighborNames(x:X): List[String] = {
    data.neigborsOf(x).toList.sortWith(compare).map(L2Printer.toCode)
  }

  /**
    Example:
    ((eax ebx ecx edi edx esi x)
    (ebx eax ecx edi edx esi)
    (ecx eax ebx edi edx esi)
    (edi eax ebx ecx edx esi x)
    (edx eax ebx ecx edi esi)
    (esi eax ebx ecx edi edx x)
    (x eax edi esi))
   */
  def hwView = sortedMembers.map{
    m => (L2Printer.toCode(m) :: sortedNeighborNames(m)).mkString("(", " ", ")")
  }.mkString("(", "\n", ")")
}
