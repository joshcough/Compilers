package L2Compiler

import L2AST._

object LivenessMain {
  import io.FileHelper._
  import java.io.File
  import L2CompilerMain._

  def main(args:Array[String]){ println(liveness(new File(args(0)).read)) }

  //  % liveness f.L2f
  //  ((in (eax) (eax x)) (out (eax x) ()))
  def liveness(code:String) = L2Printer.hwView(inoutForTesting(code))
}

object Liveness extends Liveness

// TODO: probably should fill in the usages variable.
case class LiveRange(x:X, range:Int, usages:Int=0) extends Ordered[LiveRange] {
  def compare(that: LiveRange) = this.range compare that.range
}
case class InstructionInOutSet(index: Int, inst:Instruction, gen:Set[X], kill:Set[X], in:Set[X], out:Set[X])

trait Liveness {

  val callerSave = Set[X](eax, ebx, ecx, edx)
  val x86CallerSave = Set[X](eax, ecx, edx)
  val calleeSave = Set[X](edi, esi)
  val arguments = Set[X](eax, ecx, edx)
  val result = Set[X](eax)

  def gen(i:Instruction): Set[X] = {
    def gen(rhs:AssignmentRHS): Set[X] = rhs match {
      case r:Register => Set(r)
      case Print(s) => gen(s)
      case Allocate(n, init) => gen(n)  union gen(init)
      case ArrayError(a, n)  => gen(a)  union gen(n)
      case Comp(s1, op, s2)  => gen(s1) union gen(s2)
      case MemRead(MemLoc(bp, _)) => Set(bp)
      case n:Num => Set()
      case l:Label => Set()
      case v:Variable => Set(v)
    }
    i match {
      case Assignment(x, i) => gen(i)
      case Increment(x, s)  => gen(x) union gen(s)
      case Decrement(x, s)  => gen(x) union gen(s)
      case Multiply(x, s)   => gen(x) union gen(s)
      case LeftShift(x, s)  => gen(x) union gen(s)
      case RightShift(x, s) => gen(x) union gen(s)
      case BitwiseAnd(x, s) => gen(x) union gen(s)
      case CJump(comp, l1, l2) => gen(comp)
      case MemWrite(MemLoc(bp, _), s) => Set(bp) union gen(s)
      case Goto(s) => gen(s)
      case Call(s) => gen(s) union arguments
      case TailCall(s) => gen(s) union arguments union calleeSave
      case Return => result union calleeSave
      case LabelDeclaration(_) => Set()
    }
  }

  def kill(i:Instruction): Set[X] = i match {
    case Assignment(x:X, Print(s)) => Set(x) union x86CallerSave
    case Assignment(x:X, Allocate(n, init)) => Set(x) union x86CallerSave
    case Assignment(x:X, ArrayError(a, n)) => Set(x) union x86CallerSave
    case Assignment(x:X, _) => Set(x)
    case Increment(x, _) => Set(x)
    case Decrement(x, _) => Set(x)
    case Multiply(x, _) => Set(x)
    case LeftShift(x, _) => Set(x)
    case RightShift(x, _) => Set(x)
    case BitwiseAnd(x, _) => Set(x)
    case MemWrite(_, _) => Set()
    case Goto(s) => Set()
    case CJump(comp, l1, l2) => Set()
    case Call(s) => callerSave union result
    case TailCall(s) => Set()
    case Return => Set()
    case LabelDeclaration(_) => Set()
  }

  // just gets the last inout result. (the most important one)
  def inoutFinalResult(f:Func): List[InstructionInOutSet] = inout(f.body).head

  // builds up a giant list of all the intermediate inout results
  // robby starts out with a function, and empty in and out sets for each instruction
  // that is the first result in the list return here
  // then there is a result for each slide all the way down to the last slide
  // when things are finally complete (we've reached the fixed point)
  def inout(instructions:List[Instruction]): List[List[InstructionInOutSet]] = {

    // does the next round of moving things up the in/out chains.
    // recurs until the result is the same as what we've got so far.
    def inout(acc:List[List[InstructionInOutSet]]): List[List[InstructionInOutSet]] = {

      val current = acc.head

      // returns the successors for a given instruction.
      def succ(i:InstructionInOutSet): Set[InstructionInOutSet] = {
        def succIndeces(n: Int): Set[Int] = instructions(n) match {
          case Return | TailCall(_) | Assignment(_, ArrayError(_, _)) => Set()
          case Goto(label) => Set(findLabelDecIndex(label))
          case CJump(_, l1, l2) => Set(findLabelDecIndex(l1), findLabelDecIndex(l2))
          // we have to test that there is something after this instruction
          // in case the last instruction is something other than return or cjump
          // i think that in normal functions this doesn't happen but the hw allows it.
          case _ if (instructions.isDefinedAt(n + 1)) => Set(n + 1)
          case _ => Set()
        }
        def findLabelDecIndex(label: Label): Int = {
          val index = instructions.indexOf(LabelDeclaration(label))
          if (index == -1) error("no such label: " + label.name) else index
        }
        succIndeces(i.index).map(current(_))
      }

      // in and out functions
      // in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
      def in(i:InstructionInOutSet): Set[X] = gen(i.inst) union (i.out -- kill(i.inst))
      // out(n) = ∪{in(m) | m ∈ succ(n)}
      def out(i:InstructionInOutSet): Set[X] = succ(i).flatMap(_.in)

      // build the next result
      val nextStep = current.map(i => i.copy(in=in(i), out=out(i)))

      // if we've reached the fixed point, we can stop. otherwise continue.
      if(nextStep == current) acc else inout(nextStep :: acc)
    }

    // start out with empty in and out sets for all instructions
    val emptyStartSet = instructions.zipWithIndex.map {
      case (inst, index) => InstructionInOutSet(index, inst, gen(inst), kill(inst), Set[X](), Set[X]())
    }
    // then fill them in until we reach the fixed point.
    inout(List(emptyStartSet))
  }

  def liveRanges(iioss: List[InstructionInOutSet]): List[List[LiveRange]] = {
    def liveRanges(x: X, sets: List[List[X]]): List[LiveRange] = sets match {
      case Nil => Nil
      case y::ys => {
        if(y contains x) {
          val (h,t) = sets.span(_.contains(x))
          LiveRange(x, h.size) :: liveRanges (x, t)
        } else liveRanges(x, sets.dropWhile(! _.contains(x)))
      }
    }
    val inSets = iioss.map(_.in)
    val variablesAndRegisters = inSets.foldLeft(Set[X]()){
      case (acc, s) => acc union s
    }.filterNot(x => x == ebp || x == esp)
    for(x <- variablesAndRegisters.toList.sorted) yield liveRanges(x, inSets.map(_.toList))
  }
}
