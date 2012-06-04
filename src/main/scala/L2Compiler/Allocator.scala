package L2Compiler

import L2AST._
import util.Timer

// the allocator brings together everything in L2
// for each function in the program, it tries to see if it can allocate it as is.
// if so, its fine, and leaves it alone.
// if not, it rewrites it so that edi and esi can be spilled.
// after that, it continuously tries to allocate the function.
// if it is unable to, it spills a variable, and tries again.
// it does this until either a) it works, or b) it is out of variables to spill.
// the last case results in error. 
trait Allocator extends Spill with Liveness with Interference with Timer {

  // allocates all of the functions in the given L2 program
  def allocate(ast: L2): L2 = {
    val newMain = allocate(ast.main, true)
    val l1Functions = ast.funs.map(f => timed("allocating function: " + f.name, allocate(f, false)))
    L2(newMain, l1Functions)
  }

  // gives back a fully allocated function (if its possible to allocate it)
  // with all of the variables replaced with the assigned registers.
  def allocate(f: Func, isMain:Boolean): Func = {

    val ((allocatedFunction, allocs), espOffset) = allocateCompletely(initialRewrite(f))
    // adjust the stack at the start of the function right here.
    val label = allocatedFunction.body.head
    val bodyWithoutLabel = allocatedFunction.body.tail
    val decEsp = List(Decrement(esp, Num(- espOffset)))
    val incEspMaybe = if(isMain) List(Increment(esp, Num(- espOffset))) else List()
    val finalFunction = Func(label :: decEsp ::: bodyWithoutLabel ::: incEspMaybe)
    new VariableToRegisterReplacer(allocs).replaceVarsWithRegisters(finalFunction)
  }

  // allocateCompletely rewrites (spills) until the function is colorable
  def allocateCompletely(originalF: Func): ((Func, Map[Variable, Register]), Int) =
    timed("allocating completely function: " + originalF.name, {

      val varsToSpill = originalF.body.flatMap(FindVariables.findVars).distinct
      val f = Func(varsToSpill match {
        case Nil => originalF.body
        case spillVars => spillVars.zipWithIndex.foldLeft(originalF.body) {
          case (body, (sv, i)) => spill(sv, (-i * 4) - 4, body)
        }
      })

      val io = timed("inoutFinalResult(" + f.name + ")", inoutFinalResult(f))
      attemptAllocation(io)._1 match {
        case Some(registerMap) => ((f, registerMap), (-varsToSpill.size * 4))
        case None => error("allocation impossible")
      }
    })

  // the second thing returned here is the progress we were actually able to make.
  def attemptAllocation(iioss:List[InstructionInOutSet]):
    (Option[Map[Variable, Register]], Map[Variable, Option[Register]]) = {
    timed("attemping allocation: ", attemptAllocation(buildInterferenceSet(iioss)))
  }

  def attemptAllocation(graph:InterferenceGraph):
    (Option[Map[Variable, Register]], Map[Variable, Option[Register]]) = {
    val variables: List[Variable] =
      graph.variables.toList.sortWith(_ < _).sortWith(graph.neigborsOf(_).size > graph.neigborsOf(_).size)
    val registers: Set[Register] = Set(eax, ebx, ecx, edx, edi, esi)
    val defaultPairings: Map[Variable, Option[Register]] = variables.map(v => (v, None)).toMap
    val finalPairings = variables.foldLeft(defaultPairings){ (pairs, v) =>
      val neighbors: Set[X] = graph.neigborsOf(v)
      val neighborRegisters: Set[Register] = neighbors.collect{ case r: Register => r }
      val neighborVariables: Set[Variable] = neighbors.collect{ case v: Variable => v }
      val nonNeighborRegisters: Set[Register] = registers -- neighborRegisters
      val registersNeighborsVariablesLiveIn: Set[Register] = neighborVariables.flatMap(pairs.get(_)).flatten
      val availableRegisters: List[Register] =
        (nonNeighborRegisters -- registersNeighborsVariablesLiveIn).toList.sortWith(_ < _)
      val theRegisterMaybe = availableRegisters.headOption
      pairs + (v -> theRegisterMaybe)
    }
    // see if any variables were unpaired with a register
    val anyVariablesUnpaired = finalPairings.find{ case (v, or) => ! or.isDefined }
    // if so, the graph was uncolorable. if not, return the pairings (but strip off the Option wrapper)
    if(anyVariablesUnpaired.isDefined) (None, finalPairings)
    else (Some(finalPairings.map{ case (v, or) => (v, or.get)}), finalPairings)
  }

  // sets up the function so that edi and esi can be spilled.
  def initialRewrite(f:Func): Func = {
    val z1In = Assignment(Variable("__z1"), edi)
    val z2In = Assignment(Variable("__z2"), esi)
    val z1Out = Assignment(edi, Variable("__z1"))
    val z2Out = Assignment(esi, Variable("__z2"))
    // this business arranges to make sure that edi and esi
    // get put back properly before a return or a tail-call.
    Func(f.body.head :: List(z1In,z2In) ::: f.body.drop(1).flatMap {
      i => i match {
        case Return => List(z1Out, z2Out, Return)
        case t:TailCall => List(z1Out, z2Out, t)
        case _ => List(i)
      }
    })
  }

  // chooses the variable to spill.
  // TODO: currently this works by simply taking the variable with the longest live range.
  // TODO: it doesnt look at which has the most usages. this should be resolved in the
  // TODO: event of a tie.
  def chooseSpillVar(liveRanges: List[List[LiveRange]]): Option[Variable] = {
    def maxRange(ranges:List[LiveRange]): Option[LiveRange] = ranges.sorted.headOption
    liveRanges.flatMap(maxRange).sorted.map(_.x)
            .collect{case v:Variable => v}.filterNot(_.name.startsWith("spilled_var")).headOption
  }

  def chooseSpillVars(graph: InterferenceGraph, io: List[InstructionInOutSet]): List[Variable] = {
    val sixes = graph.variables.
            filterNot(_.name.startsWith("spilled_var")).
            filter(v => graph.neigborsOf(v).size >= 6).
            toList
    sixes match {
      case Nil => chooseSpillVar(liveRanges(io)).toList
      case _ => sixes
    }
  }
}


// replaces variables with registers in an L2 function.
object FindVariables {

  def findVars(i:Instruction): List[Variable] = i match {
    case Assignment(x1, x2) => findVars(x1) ::: findVars(x2)
    case Increment(s1, s2) => findVars(s1) ::: findVars(s2)
    case Decrement(s1, s2) => findVars(s1) ::: findVars(s2)
    case Multiply(s1, s2) => findVars(s1) ::: findVars(s2)
    case BitwiseAnd(s1, s2) => findVars(s1) ::: findVars(s2)
    case LeftShift(s1, s2) => findVars(s1) ::: findVars(s2)
    case RightShift(s1, s2) => findVars(s1) ::: findVars(s2)
    case MemWrite(loc, x) => findVars(loc) ::: findVars(x)
    case CJump(comp, l1, l2) => findVars(comp)
    case Call(x) => findVars(x)
    case TailCall(x) => findVars(x)
    case ld:LabelDeclaration => Nil
    case Return => Nil
    case g:Goto => Nil
  }

  def findVars(s:S): List[Variable] = s match {
    case v:Variable => List(v)
    case _ => Nil
  }

  def findVars(rhs:AssignmentRHS): List[Variable] = rhs match {
    case MemRead(loc) => findVars(loc)
    case Print(s) => findVars(s)
    case Allocate(n, init) => findVars(n) ::: findVars(init)
    case ArrayError(a, n) => findVars(a) ::: findVars(n)
    case c:Comp => findVars(c)
    case s:S => findVars(s)
  }

  private def findVars(loc:MemLoc): List[Variable] = findVars(loc.basePointer)
  private def findVars(c:Comp): List[Variable] = findVars(c.s1) ::: findVars(c.s2)
}

// replaces variables with registers in an L2 function.
class VariableToRegisterReplacer(replacements:Map[Variable, Register]) {

  def replaceVarsWithRegisters(f:Func): Func = Func(f.body.map(replaceVarsWithRegisters))

  private def getRegister(x:X): Register = x match {
    case v:Variable => replacements(v)
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
    case TailCall(x) => TailCall(replaceVarsWithRegisters(x))
    case ld:LabelDeclaration => ld
    case Return => Return
  }

  def replaceVarsWithRegisters(s:S): S = s match {
    case v:Variable => getRegister(v)
    case _ => s
  }

  def replaceVarsWithRegisters(rhs:AssignmentRHS):AssignmentRHS = rhs match {
    case MemRead(loc) => MemRead(replaceVarsWithRegisters(loc))
    case Print(s) => Print(replaceVarsWithRegisters(s))
    case Allocate(n, init) => Allocate(replaceVarsWithRegisters(n), replaceVarsWithRegisters(init))
    case ArrayError(a, n) => ArrayError(replaceVarsWithRegisters(a), replaceVarsWithRegisters(n))
    case c:Comp => replaceVarsWithRegisters(c)
    case s:S => replaceVarsWithRegisters(s)
  }

  private def replaceVarsWithRegisters(loc:MemLoc): MemLoc =
    MemLoc(getRegister(loc.basePointer),loc.offset)
  private def replaceVarsWithRegisters(c:Comp): Comp =
    Comp(replaceVarsWithRegisters(c.s1), c.op, replaceVarsWithRegisters(c.s2))
}