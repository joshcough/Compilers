package L2Compiler

import L2AST._
import L2Printer._

case class LiveRange(x:X, range:Int)

trait Liveness {

  val callerSave = Set[X](eax, ebx, ecx, edx)
  val calleeSave = Set[X](edi, esi)
  val arguments = Set[X](eax, edx, ecx)
  val result = Set[X](eax)

  def gen(i:Instruction): Set[X] = {
    def gen(rhs:AssignmentRHS): Set[X] = rhs match {
      case r:Register => Set(r)
      case Print(s) => gen(s)
      case Allocate(n, init) => gen(n)  union gen(init)
      case ArrayError(a, n)  => gen(a)  union gen(n)
      case Comp(s1, op, s2)  => gen(s1) union gen(s1)
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

  def kill(i:Instruction): Set[X] = {
    def kill(rhs:AssignmentRHS): Set[X] = rhs match {
      case r:Register => Set()
      case Print(s) => Set(eax) // hmmm...i thought others were killed here as well.
      case Allocate(n, init) => Set(eax) // same here
      case ArrayError(a, n)  => Set(eax) // same here
      case Comp(s1, op, s2)  => Set()
      case MemRead(MemLoc(bp, _)) => Set()
      case n:Num => Set()
      case l:Label => Set()
    }
    i match {
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
  }

  type Index = Int
  case class InstructionInOutSet(index: Index, inst:Instruction, in:Set[X], out:Set[X]){
    override def toString = "(" + toCode(inst) + " " + toCode(in) + " " + toCode(out) + ")"
  }

  // just gets the last inout result. (the most important one)
  def inoutFinalResult(f:Func): List[InstructionInOutSet] = inout(f).last

  // builds up a giant list of all the intermediate inout results
  // robby starts out with a function, and empty in and out sets for each instruction
  // that is the first result in the list return here
  // then there is a result for each slide all the way down to the last slide
  // when things are finally complete (we've reached the fixed point)
  def inout(f:Func): List[List[InstructionInOutSet]] = {
    class InOutHelper(f:Func) {
      // TODO: consider just putting the label dec in the body
      def instructions: List[Instruction] = f.name :: f.body

      def succIndeces(n:Index): Set[Index] = instructions(n) match {
        case Return|TailCall(_) => Set()
        case CJump(_, l1, l2) => Set(findLabelDecIndex(l1), findLabelDecIndex(l1))
        case _ => Set(n+1)
      }

      def findLabelDecIndex(label:Label):Index = {
        val index = f.body.indexOf(LabelDeclaration(label))
        if(index == -1) error("no such label: " + label.name) else index
      }

      def inout: List[List[InstructionInOutSet]] = {
        // start out with empty in and out sets for all instructions
        val emptyStartSet = (f.name :: f.body).zipWithIndex.map {
          case (inst, index) => InstructionInOutSet(index, inst, Set[X](), Set[X]())
        }
        // then fill them in until we reach the fixed point.
        inout(List(emptyStartSet))
      }

      /**
       * in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
       * out(n) = ∪{in(m) | m ∈ succ(n)}
       */
      // does the next round of moving things up the in/out chains.
      // recurs until the result is the same as what we've got so far.
      private def inout(acc:List[List[InstructionInOutSet]]): List[List[InstructionInOutSet]] = {

        val current = acc.last

        // in and out functions
        def in(i:InstructionInOutSet): Set[X] = gen(i.inst) union (i.out -- kill(i.inst))
        def out(i:InstructionInOutSet): Set[X] = {
          val indices = succIndeces(i.index)
          if(indices.isEmpty) Set() else indices.map(current(_).in).reduceLeft(_ union _)
        }

        // build the next result
        val nextStep: List[InstructionInOutSet] = current.foldRight(List[InstructionInOutSet]()){
          case (i, c) => InstructionInOutSet(i.index, i.inst, in(i), out(i)) :: c
        }

        // if we've reached the fixed point, we can stop. otherwise continue.
        if(nextStep == current) acc else inout(acc :+ nextStep)
      }
    }
    new InOutHelper(f).inout
  }

  /**
    TODO: - not yet using the kill set as part of interference!
    TODO: cx <- instructions
    TODO:
    Constrained arithmetic operators
    Add interference edges to disallow the illegal registers
    when building the interference graph, before starting the
    coloring.
    E.g., if you have this instruction (a <- y < x) then
    add edges between a and the registers edi and esi,
    ensuring a ends up in eax, ecx, edx, ebx, or spilled
   */
  def buildInterferenceSet(iioss: List[InstructionInOutSet]): Set[(X,X)] = {
    val ins = iioss.map(_.in)
    val outs = iioss.map(_.out)
    val in_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    val out_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    in_interference ++ out_interference
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
    val variablesAndRegisters = inSets.foldLeft(Set[X]()){ case (acc, s) => acc union s}
    for(x <- variablesAndRegisters.toList) yield liveRanges(x, inSets.map(_.toList))
  }  
}


// this was in the inout function:
//val (head::rest) = inout(empty, 0, stopAfterNSteps)
//val newHead: InstructionInOutSet = head.copy(in = head.in - edi - esi)
//newHead :: rest
//head :: rest

//      val next = acc.foldRight((List[InstructionInOutSet](), Set[X]())){
//        case (iios, (acc, lastIn)) => {
//          val newIn = in(iios)
//          val newOut = out(iios)
//          //println(iios + ", " + toCode(newIn) + ", " + toCode(newOut))
//          (InstructionInOutSet(iios.index, iios.inst, newIn, newOut) :: acc, newIn)
//        }
//      }._1