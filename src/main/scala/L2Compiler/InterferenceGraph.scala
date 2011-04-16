package L2Compiler

import L2AST._
import Liveness.kill

object InterferenceGraph {
  def base: InterferenceGraph = {
    InterferenceGraph(new BiDirectionalGraph(Set(
      eax -> ebx, eax -> ecx, eax -> edi, eax -> edx, eax -> esi,
      ebx -> ecx, ebx -> edi, ebx -> edx, ebx -> esi,
      ecx -> edi, ecx -> edx, ecx -> esi,
      edi -> edx, edi -> esi,
      edx -> esi)))
  }

  /**
    TODO: cx <- instructions
    TODO:
    Constrained arithmetic operators
    Add interference edges to disallow the illegal registers
    when building the interference graph, before starting the
    coloring.
    E.g., if you have this instruction (a <- y < x) then
    add edges between a and the registers edi and esi,
    ensuring a ends up in eax, ecx, edx, ebx, or spilled
   
    The (cx <- s cmp s) instruction in L1 is limited to
    only 4 possible destinations.
    The (x sop= sx) instruction in L1 is limited to only
    shifting by the value of ecx (or by a constant in the
    other form)

    Build interference graph from the liveness information
        Two variable live at the same time interfere with each other
        Killed variables interferes with all live variables at that
        point, unless it is a (x <- y) instruction (in which
        case it is fine if x and y share a register)
        All real registers interfere with each other
   */
  def buildInterferenceSet(iioss: List[InstructionInOutSet]): InterferenceGraph = {
    base.addInterference(iioss.flatMap { iios: InstructionInOutSet =>
      val in_interference: Set[(X,X)] = for(x <- iios.in; y <- iios.in; if(x!=y)) yield (x,y)
      val out_interference: Set[(X,X)] = {
        // add in the kill
        val outsPlusKill = (iios.out ++ kill(iios.inst))
        for(x <- outsPlusKill; y <- outsPlusKill; if(x!=y)) yield (x,y)
      }
      val special_interference: Set[(X, X)] = iios match {
        case _ => Set()
      }
      in_interference ++ out_interference ++ special_interference
    }.toSet)
  }

  def chooseRegisters(graph:InterferenceGraph): Option[Map[Variable, Register]] = {
    val variables: Set[Variable] = graph.data.members.collect{ case v: Variable => v }
    val registers: Set[Register] = Set(eax, ebx, ecx, edx, edi, esi)
    val defaultPairings: Map[Variable, Option[Register]] = variables.map(v => (v, None)).toMap
    val finalPairings = variables.foldLeft(defaultPairings){ (pairs, v) =>
      //println("--------------------------------------")
      //println("v=" + v)
      val neighbors: Set[X] = graph.data.neigborsOf(v)
      //println("neighbors=" + neighbors)
      val neighborRegisters: Set[Register] = neighbors.collect{ case r: Register => r }
      //println("neighborRegisters=" + neighborRegisters)
      val neighborVariables: Set[Variable] = neighbors.collect{ case v: Variable => v }
      //println("neighborVariables=" + neighborVariables)
      val nonNeighborRegisters: Set[Register] = registers -- neighborRegisters
      //println("nonNeighborRegisters=" + nonNeighborRegisters)
      val registersNeighborsVariablesLiveIn: Set[Register] = neighborVariables.flatMap(pairs.get(_)).flatten
      //println("registersNeighborsVariablesLiveIn=" + registersNeighborsVariablesLiveIn)
      val availableRegisters: List[Register] =
        (nonNeighborRegisters -- registersNeighborsVariablesLiveIn).toList.sortWith(_<_)
      //println("availableRegisters=" + availableRegisters)
      val theRegisterMaybe = availableRegisters.headOption
      //println("theRegisterMaybe=" + theRegisterMaybe)
      pairs + (v -> theRegisterMaybe)
    }
    //println("finalPairings=" + finalPairings)
    // see if any variables were unpaired with a register
    val anyVariablesUnpaired = finalPairings.find{ case (v, or) => ! or.isDefined }
    // if so, the graph was uncolorable. if not, return the pairings (but strip off the Option wrapper)
    if(anyVariablesUnpaired.isDefined) None else Some(finalPairings.map{ case (v, or) => (v, or.get)})
  }

  /**
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
  **/
}

case class InterferenceGraph(data:BiDirectionalGraph[X]){
  def sortedMembers = data.members.toList.sorted

  // TODO: this could all get cleaned up.
  def addInterference(connections:(X,X)*): InterferenceGraph = addInterference(connections.toSet)
  def addInterference(connections:Set[(X,X)]): InterferenceGraph = connections.toList match {
    case Nil => this
    // dont bother adding ebp or esp to the graph. 
    case (x1,x2)::xs if (x1 == ebp || x2 == ebp || x1 == esp || x2 == esp) => this.addInterference(xs.toSet)
    case (x1,x2)::xs => InterferenceGraph(data + (x1 -> x2)).addInterference(xs.toSet)
  }

  def sortedNeighborNames(x:X): List[String] = {
    data.neigborsOf(x).toList.sorted.map(L2Printer.toCode)
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