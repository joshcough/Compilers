package L2Compiler

import L2AST._
import L2Printer._

case class LiveRange(x:X, range:Int)

// i have to write a real succ function. most succs are the instruction that follows
// but cjump is diferent. succ(n) returns a set of instructions, not a single instruction.
// this is because cjump has two succs.

/**
  TODO:
  Constrained arithmetic operators
  Add interference edges to disallow the illegal registers
  when building the interference graph, before starting the
  coloring.
  E.g., if you have this instruction (a <- y < x) then
  add edges between a and the registers edi and esi,
  ensuring a ends up in eax, ecx, edx, ebx, or spilled
 */

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

  case class InstuctionInOutSet(i:Instruction, in:Set[X], out:Set[X]){
    override def toString = "(" + toCode(i) + " " + toCode(in) + " " + toCode(out) + ")"
  }

  def inout(f:Func, stopAfterNSteps:Option[Int]=None): List[InstuctionInOutSet] = {
    // start out with empty in and out sets for all instructions
    val empty = (f.name :: f.body).map(InstuctionInOutSet(_, Set[X](), Set[X]()))
    // then fill them in until we reach the fixed point.
    inout(empty, 0, stopAfterNSteps)
    //val (head::rest) = inout(empty, 0, stopAfterNSteps)
    //val newHead: InstuctionInOutSet = head.copy(in = head.in - edi - esi)
    //newHead :: rest
    //head :: rest
  }

  /**
   * in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
   * out(n) = ∪{in(m) | m ∈ succ(n)}
   */
  // does the next round of moving things up the in/out chains.
  // recurs until the result is the same as what we've got so far.
  private def inout(acc:List[InstuctionInOutSet], currentStep: Int, stopAfterNSteps:Option[Int]): List[InstuctionInOutSet] = {
    def in(iios:InstuctionInOutSet) = gen(iios.i) union (iios.out -- kill(iios.i))
    val next = acc.foldRight((List[InstuctionInOutSet](), Set[X]())){
      case (iios, (acc, lastIn)) => {
        val newIn = in(iios)
        //println(iios + ", " + toCode(newIn) + ", " + toCode(lastIn))
        (InstuctionInOutSet(iios.i, newIn, lastIn) :: acc, newIn)
      }
    }._1
    stopAfterNSteps match {
      case Some(n) if(n==currentStep+1) => next
      case _ => if(next == acc) acc else inout(next, currentStep + 1, stopAfterNSteps)
    }
  }

  /**
   * TODO - not yet using the kill set as part of interference!
   * TODO cx <- instructions
   */
  def buildInterferenceSet(iioss: List[InstuctionInOutSet]): Set[(X,X)] = {
    val ins = iioss.map(_.in)
    val outs = iioss.map(_.out)
    val in_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    val out_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    in_interference ++ out_interference
  }

  def liveRanges(iioss: List[InstuctionInOutSet]): List[List[LiveRange]] = {
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
