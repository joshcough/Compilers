package L2Compiler

import L2AST._

// ebx, esi, and edi are callee / function save
// eax, edx, and ecx are caller / application save / arguments (in that order)
trait Liveness {

  def gen(i:Instruction): Set[X] = {
    def g(i: Instruction, s2: S) = gen(i) union gen(s2)
    i match {
      case Num(_) => Set()
      case x: X => Set(x)
      case Allocate(n:S, init:S) => g(n,init)
      case Assignment(x, i) => gen(i)
      case Increment(x, s) => g(x, s)
      case Decrement(x, s) => g(x, s)
      case Multiply(x, s) => g(x, s)
      case LeftShift(x, s) => g(x, s)
      case RightShift(x, s) => g(x, s)
      case BitwiseAnd(x, s) => g(x, s)
      case Comp(s1, op, s2) => g(s1,s2)
      case CJump(comp, l1, l2) => gen(comp)
      case MemLoc(bp, _) => Set(bp)
      case MemRead(loc) => gen(loc)
      case MemWrite(loc, s:S) => g(loc,s)
      case Print(s:S) => gen(s) union Set(eax)
      case Goto(s: S) => gen(s)
      case Call(s:S) => gen(s)
      case Return => Set(ebx, edi, esi)
      case _ => Set()
    }
  }
  def kill(i:Instruction): Set[X] = i match {
    case Allocate(n:S, init:S) => Set(eax)
    case Assignment(x, _) => Set(x)
    case Increment(x, _) => Set(x)
    case Decrement(x, _) => Set(x)
    case Multiply(x, _) => Set(x)
    case LeftShift(x, _) => Set(x)
    case RightShift(x, _) => Set(x)
    case BitwiseAnd(x, _) => Set(x)
    case MemLoc(bp, _) => Set()
    case MemRead(loc) => Set()
    case MemWrite(loc, s:S) => Set()
    case Print(s:S) => Set(eax)
    case Goto(s: S) => Set()
    case Comp(s1, op, s2) => Set()
    case CJump(comp, l1, l2) => Set()
    case Call(s:S) => Set(eax, edx, ecx)
    case Return => Set()
    case _ => Set()
  }

  /**
      in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
      out(n) = ∪{in(m) | m ∈ succ(n)}
    */
  case class InstuctionInOutSet(i:Instruction, in:Set[X], out:Set[X])
  def inout(f:L2Function): List[InstuctionInOutSet] = {
    val (head::rest) = inout((f.name :: f.body).map(InstuctionInOutSet(_, Set[X](), Set[X]())))
    val newHead = head.copy(in = head.in - ebx - edi -esi)
    newHead :: rest
  }
  
  def inoutHack(ins:List[Instruction]): List[InstuctionInOutSet] = {
    inout(ins.map(InstuctionInOutSet(_, Set[X](), Set[X]())))
  }
  def inout(acc:List[InstuctionInOutSet]): List[InstuctionInOutSet] = {
    def in(iios:InstuctionInOutSet) = gen(iios.i) union (iios.out -- kill(iios.i))
    val next = acc.foldRight((List[InstuctionInOutSet](), Set[X]())){
      case (iios, (acc, lastIn)) => {
        val newIn = in(iios)
        (InstuctionInOutSet(iios.i, newIn, lastIn) :: acc, newIn)
      }
    }._1
    if(next == acc) acc else inout(next)
  }

  def buildInterferenceSet(iioss: List[InstuctionInOutSet]): Set[(X,X)] = {
    val ins = iioss.map(_.in)
    val outs = iioss.map(_.out)
    val in_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    val out_interference = ins.flatMap{ s => for(x <- s; y <- s; if(x!=y)) yield (x,y) }.toSet
    in_interference ++ out_interference
  }
}