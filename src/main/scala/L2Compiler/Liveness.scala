package L2Compiler

import L2AST._

case class LiveRange(x:X, range:Int)

// TODO: ebx is a caller save register now.
// ebx, esi, and edi are callee / function save
// eax, edx, and ecx are caller / application save / arguments (in that order)

// i have to write a real succ function. most succs are the instruction that follows
// but cjump is diferent. succ(n) returns a set of instructions, not a single instruction.
// this is because cjump has two succs.

// TODO: better really double check all these against the slides...

trait Liveness {

  val callerSave = Set(eax, ebx, ecx, edx)

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
      case Call(s) => gen(s)
      case TailCall(s) => gen(s) union Set(edi, esi)
      case Return => Set(eax, edi, esi) // TODO: i think eax needs to be in here...(the slides say result and callee save)
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
      case Assignment(x:Register, _) => Set(x)
      case Assignment(_, _) => Set()
      case Increment(x, _) => Set(x)
      case Decrement(x, _) => Set(x)
      case Multiply(x, _) => Set(x)
      case LeftShift(x, _) => Set(x)
      case RightShift(x, _) => Set(x)
      case BitwiseAnd(x, _) => Set(x)
      case MemWrite(_, _) => Set()
      case Goto(s) => Set()
      case CJump(comp, l1, l2) => Set()
      case Call(s) => Set(eax, ebx, ecx, edx)
      case TailCall(s) => Set(eax, ebx, ecx, edx) // this is an educated guess
      case Return => Set()
      case LabelDeclaration(_) => Set()
    }
  }

  /**
   * in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
   * out(n) = ∪{in(m) | m ∈ succ(n)}
   */
  case class InstuctionInOutSet(i:Instruction, in:Set[X], out:Set[X])
  def inout(f:Func): List[InstuctionInOutSet] = {
    val (head::rest) = inout((f.name :: f.body).map(InstuctionInOutSet(_, Set[X](), Set[X]())))
    val newHead = head.copy(in = head.in - ebx - edi - esi)
    newHead :: rest
  }

  private def inout(acc:List[InstuctionInOutSet]): List[InstuctionInOutSet] = {
    def in(iios:InstuctionInOutSet) = gen(iios.i) union (iios.out -- kill(iios.i))
    val next = acc.foldRight((List[InstuctionInOutSet](), Set[X]())){
      case (iios, (acc, lastIn)) => {
        val newIn = in(iios)
        (InstuctionInOutSet(iios.i, newIn, lastIn) :: acc, newIn)
      }
    }._1
    if(next == acc) acc else inout(next)
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
