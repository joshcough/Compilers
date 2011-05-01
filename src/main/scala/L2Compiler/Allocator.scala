package L2Compiler

import L2Compiler.L2AST._

trait Allocator extends Spill with Liveness with Interference with L2Printer {

  def allocate(ast: L2): L2 = {
    val l1Functions = (ast.main :: ast.funs).map(allocateCompletely)
    L2(l1Functions.head, l1Functions.tail)
  }

  def allocateCompletely(f: Func): Func = {
    // first, try to see if we can do allocation without any rewriting
    val (finalFunction, allocs) = attemptAllocation(inoutFinalResult(f))._1 match {
      case Some(registerMap) => (f, registerMap)
      case _ => {
        // then if it fails, rewrite until we can color
        def allocateCompletely(f: Func, offset: Int): ((Func, Map[Variable, Register]), Int) = {
          attemptAllocation(inoutFinalResult(f))._1 match {
            case Some(registerMap) => ((f, registerMap), offset)
            case None => chooseSpillVar(liveRanges(inoutFinalResult(f))) match {
              case Some(sv) => allocateCompletely(Func(spill(sv, offset - 4, f.body)), offset - 4)
              case None => error("allocation impossible")
            }
          }
        }
        val ((allocatedFunction, allocs), espOffset) = allocateCompletely(initialRewrite(f), 0)
        // adjust the stack at the start of the function right here.
        // if we never spilled, we dont have to do anything
        if(espOffset == 0) (allocatedFunction, allocs)
        // but if we did, adjust right after the label.
        else {
          val label = allocatedFunction.body.head
          val bodyWithoutLabel = allocatedFunction.body.tail
          val decEsp = List(Decrement(esp, Num(- espOffset)))
          val incEspMaybe = if(allocatedFunction.isMain) List(Increment(esp, Num(- espOffset))) else List()
          (Func(label :: decEsp ::: bodyWithoutLabel ::: incEspMaybe), allocs)
        }
      }
    }
    new VariableToRegisterReplacer(allocs).replaceVarsWithRegisters(finalFunction)
  }

  def initialRewrite(f:Func): Func = {
    val z1In = Assignment(Variable("__z1"), edi)
    val z2In = Assignment(Variable("__z2"), esi)
    val z1Out = Assignment(edi, Variable("__z1"))
    val z2Out = Assignment(esi, Variable("__z2"))
    Func(f.body.head :: List(z1In,z2In) ::: f.body.drop(1).flatMap {
      i => i match {
        case Return => List(z1Out,z2Out, Return)
        case t:TailCall => List(z1Out,z2Out, t)
        case _ => List(i)
      }
    })
  }

  def chooseSpillVar(liveRanges: List[List[LiveRange]]): Option[Variable] = {
    def maxRange(ranges:List[LiveRange]): Option[LiveRange] = ranges.sorted.headOption
    liveRanges.flatMap(maxRange).sorted.map(_.x)
            .collect{case v:Variable => v}.filterNot(_.name.startsWith("spilled_var")).headOption
  }
}


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