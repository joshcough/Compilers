package L2Compiler

import L2AST._

object InterferenceMain {
  import L2CompilerMain._
  import io.FileHelper._

  def main(args:Array[String]){
    val (i,a) = interferenceAndAllocation(new java.io.File(args(0)).read)
    println(i + "\n" + a)
  }

  def interferenceAndAllocation(code:String) = {
    val inouts = inoutForTesting(code, None)
    (buildInterferenceSet(inouts).hwView, printAllocation(attemptAllocation(inouts)._1))
  }

  def graphTest(code:String): String = {
    val (i,a) = interferenceAndAllocation(code)
    i + "\n" + a
  }
}

class InterferenceGraph(private val map:Map[X, Set[X]]){
  def this() = this(Map())

  def addNodes(xs:X*):InterferenceGraph = xs.foldLeft(this){ case (g, x) => g.addNode(x) }

  def addNode(x:X):InterferenceGraph = {
    if(map.contains(x)) this else new InterferenceGraph(map + (x -> Set()))
  }
  def addEdge(x1:X, x2:X): InterferenceGraph = {
    // dont bother adding ebp or esp
    if(x1 == ebp || x1 == esp || x2 == ebp || x2 == esp) this
    // dont bother adding interference edges between a variable or register and itself...duh
    else if (x1 == x2) this
    else{
      val x1Connections = map.getOrElse(x1, Set()) + x2
      val x2Connections = map.getOrElse(x2, Set()) + x1
      new InterferenceGraph(map + (x1 -> x1Connections) + (x2 -> x2Connections))
    }
  }

  def addEdges(connections:(X,X)*): InterferenceGraph = {
    connections.foldLeft(this){ case (g, (x1, x2)) => g.addEdge(x1, x2) }
  }

  def members = map.keySet
  def variables = members.collect{ case v: Variable => v }.toSet
  def neigborsOf(x:X): Set[X] = map(x)

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
  def hwView = {
    def sortedMembers = members.toList.sorted
    def sortedNeighborNames(x:X): List[String] = map(x).toList.sorted.map(L2Printer.toCode)
    sortedMembers.map{ m =>
      (L2Printer.toCode(m) :: sortedNeighborNames(m)).mkString("(", " ", ")")
    }.mkString("(", "\n", ")")
  }
}

trait Interference {

  val registerInterference: InterferenceGraph = {
    new InterferenceGraph().addEdges(
      eax -> ebx, eax -> ecx, eax -> edi, eax -> edx, eax -> esi,
      ebx -> ecx, ebx -> edi, ebx -> edx, ebx -> esi,
      ecx -> edi, ecx -> edx, ecx -> esi,
      edi -> edx, edi -> esi,
      edx -> esi)
  }

  /**
    Build interference graph from the liveness information
      Two variables live at the same time interfere with each other
      Killed variables interfere with variables in the out set
      Except that the variables x and y do not interfere if the instruction was (x <- y)
      All real registers interfere with each other
   */
  def buildInterferenceSet(iioss: List[InstructionInOutSet]): InterferenceGraph = {

    // if there is a first instruction (i certainly imagine there should be)
    // then we have to take the interference from its in set.
    val firstInstructionInSetInterference: Set[(X,X)] = iioss.headOption match {
      case Some(iios) => for(x <- iios.in; y <- iios.in; if(x!=y)) yield (x,y)
      case _ => Set()
    }

    // we always take the interference from the out sets.
    val outAndSpecialInterference: List[(X,X)] = iioss.flatMap { iios: InstructionInOutSet =>
      val out_interference: Set[(X,X)] = {
        // add in the kill
        val outsPlusKill = (iios.out ++ iios.kill)
        val initial = for(x <- outsPlusKill; y <- outsPlusKill; if(x<y)) yield (x,y)
        val assignmentRemovals = iios.inst match {
          case Assignment(v1:Variable, x:X) if(v1 != x) => Some(if(v1<x) (v1, x) else (x, v1))
          case Assignment(r: Register, v: Variable) => Some(if(r<v) (r, v) else (v, r))
          case _ => None
        }
        assignmentRemovals match {
          case Some(r) => initial -- Set(r)
          case _ => initial
        }
      }
      //  Constrained arithmetic operators
      //  Add interference edges to disallow the illegal registers
      //  when building the interference graph, before starting the coloring.
      val special_interference: Set[(X, X)] = iios.inst match {
        // if you have this instruction (a <- y < x) then
        // add edges between a and the registers edi and esi,
        // ensuring a ends up in eax, ecx, edx, ebx, or spilled
        // The (cx <- s cmp s) instruction in L1 is limited to only 4 possible destinations.
        case Assignment(v:Variable, _:Comp) => Set((v, edi), (v, esi))
        // The (x sop= sx) instruction in L1 is limited to only
        // shifting by the value of ecx (or by a constant in the other form)
        case LeftShift(_,  x:X) => Set((x, eax), (x, ebx), (x, edi), (x, edx), (x, esi))
        case RightShift(_, x:X) => Set((x, eax), (x, ebx), (x, edi), (x, edx), (x, esi))
        case _ => Set()
      }
      out_interference ++ special_interference
    }

    val interference = firstInstructionInSetInterference  ++ outAndSpecialInterference.toSet

    val vs = iioss.flatMap { iios => variables(iios.inst) }
    registerInterference.addNodes(vs:_*).addEdges(interference.toList:_*)
  }

  def variables(i:Instruction): Set[Variable] = {
    def variables(rhs:AssignmentRHS): Set[Variable] = rhs match {
      case v:Variable => Set(v)
      case Print(s) => variables(s)
      case Allocate(n, init) => variables(n)  union variables(init)
      case ArrayError(a, n)  => variables(a)  union variables(n)
      case Comp(s1, op, s2)  => variables(s1) union variables(s2)
      case MemRead(MemLoc(bp, _)) => variables(bp)
      case r:Register => Set()
      case n:Num => Set()
      case l:Label => Set()
    }
    i match {
      case Assignment(x, i) => variables(x) union variables(i)
      case Increment(x, s)  => variables(x) union variables(s)
      case Decrement(x, s)  => variables(x) union variables(s)
      case Multiply(x, s)   => variables(x) union variables(s)
      case LeftShift(x, s)  => variables(x) union variables(s)
      case RightShift(x, s) => variables(x) union variables(s)
      case BitwiseAnd(x, s) => variables(x) union variables(s)
      case CJump(comp, l1, l2) => variables(comp)
      case MemWrite(MemLoc(bp, _), s) => variables(bp) union variables(s)
      case Call(s) => variables(s)
      case TailCall(s) => variables(s)
      case Goto(_) => Set()
      case Return => Set()
      case LabelDeclaration(_) => Set()
    }
  }
}