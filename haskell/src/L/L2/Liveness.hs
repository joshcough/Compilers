{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Liveness where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.L1L2AST

data InstructionInOutSet = InstructionInOutSet {
  index  :: Int,       inst   :: L2Instruction,
  genSet :: S.Set L2X, kill   :: S.Set L2X,
  inSet  :: S.Set L2X, outSet :: S.Set L2X
}

set :: AsL2X x => [x] -> S.Set L2X
set = S.fromList . fmap asL2X

callerSave    = set [eax, ebx, ecx, edx]
x86CallerSave = set [eax, ecx, edx]
calleeSave    = set [edi, esi]
arguments     = set [eax, ecx, edx]
result        = set [eax]

class HasGen a where
  gen :: a -> S.Set L2X

instance HasGen L2X where
  gen (RegL2X r) = set [r]
  gen (VarL2X v) = set [v]

instance HasGen L2S where
  gen (XL2S x)        = gen x
  gen (NumberL2S n)   = S.empty
  gen (LabelL2S l)    = S.empty

instance HasGen (AssignRHS L2X L2S) where
  gen (CompRHS (Comp s1 _ s2)) = S.unions [gen s1, gen s2]
  gen (Allocate n init)        = S.unions [gen n,  gen init]
  gen (Print s)                = gen s
  gen (ArrayError a n)         = S.unions [gen a,  gen n]
  gen (MemRead (MemLoc bp _))  = gen bp
  gen (SRHS s)                 = gen s

instance HasGen L2Instruction where
  gen (Assign x rhs)             = gen rhs
  gen (MathInst x _ s)           = S.unions [gen x,  gen s]
  gen (MemWrite (MemLoc bp _) s) = S.unions [gen bp, gen s]
  gen (Goto s)                   = S.empty
  gen (CJump (Comp s1 _ s2) _ _) = S.unions [gen s1, gen s2]
  gen (LabelDeclaration _)       = S.empty
  gen (Call s)                   = S.unions [gen s,  arguments]
  gen (TailCall s)               = S.unions [gen s,  arguments, calleeSave]
  gen Return                     = S.unions [result, calleeSave]


{--
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
    val instructionsWithIndex = instructions.zipWithIndex
    val indeces = instructionsWithIndex.toMap
    def findLabelDecIndex(label: Label): Int =
      indeces.getOrElse(LabelDeclaration(label), error("no such label: " + label.name))
    val succIndeces = instructionsWithIndex.map {
      case (i, n) => i match {
        case Return | TailCall(_) | Assignment(_, ArrayError(_, _)) => Set()
        case Goto(label) => Set(findLabelDecIndex(label))
        case CJump(_, l1, l2) => Set(findLabelDecIndex(l1), findLabelDecIndex(l2))
        // we have to test that there is something after this instruction
        // in case the last instruction is something other than return or cjump
        // i think that in normal functions this doesn't happen but the hw allows it.
        case _ if (instructions.isDefinedAt(n + 1)) => Set(n + 1)
        case _ => Set()
      }
    }.toIndexedSeq

    // does the next round of moving things up the in/out chains.
    // recurs until the result is the same as what we've got so far.
    def inout(acc:List[List[InstructionInOutSet]]): List[List[InstructionInOutSet]] = {
      val current = acc.head
      var changes: Boolean = false
      // build the next result
      val nextStep = current.map(i => {
        // in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
        val newIn = i.gen union (i.out -- i.kill)
        // out(n) = ∪{in(m) | m ∈ succ(n)}
        val newOut = succIndeces(i.index).map(current(_)).flatMap(_.in)
        if(newIn.size > i.in.size || newOut.size > i.out.size) changes = true
        i.copy(in=newIn, out=newOut)
      })
      // if we've reached the fixed point, we can stop. otherwise continue.
      // if(nextStep == current) acc else inout(nextStep :: acc)
      if(!changes) acc else inout(nextStep :: acc)
    }

    // start out with empty in and out sets for all instructions
    val emptyStartSet = instructionsWithIndex.map {
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
--}

{--


// TODO: probably should fill in the usages variable.
case class LiveRange(x:X, range:Int, usages:Int=0) extends Ordered[LiveRange] {
  def compare(that: LiveRange) = this.range compare that.range
}


import L2AST._
import util.Timer

object LivenessMain {
  import io.FileHelper._
  import java.io.File
  import L2CompilerMain._

  def main(args:Array[String]){ println(liveness(new File(args(0)).read)) }

  //  % liveness f.L2f
  //  ((in (eax) (eax x)) (out (eax x) ()))
  def liveness(code:String) = L2Printer.hwView(inoutForTesting(code))
--}
