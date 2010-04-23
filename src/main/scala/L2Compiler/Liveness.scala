package L2Compiler

import L2AST._

trait Liveness {

  def gen(i:Instruction): List[X] = {
    def g(i: Instruction, s2: S) = (gen(i) ::: gen(s2)).removeDuplicates
    i match {
      case Num(_) => Nil
      case x: X => List(x)

      case Assignment(x, i) => gen(i)
      case Multiply(x, s) => g(x, s)
      case Increment(x, s) => g(x, s)
      case Decrement(x, s) => g(x, s)
      case LeftShift(x, s) => g(x, s)
      case RightShift(x, s) => g(x, s)
      case BitwiseAnd(x, s) => g(x, s)
      case Comp(s1, op, s2) => g(s1,s2)
      case CJump(comp, l1, l2) => gen(comp)

      case MemLoc(bp, _) => List(bp)
      case MemRead(loc) => gen(loc)
      case MemWrite(loc, s:S) => g(loc,s)
      case Print(s:S) => gen(s) :+ eax
      case Allocate(n:S, init:S) => g(n,init)
      case Goto(s: S) => gen(s)
      case Return => Nil // for now
    }
  }
  
  def kill(i:Instruction): List[X] = i match {
    case Assignment(x, _) => List(x)
    case Multiply(x, _) => List(x)
    case Increment(x, _) => List(x)
    case Decrement(x, _) => List(x)
    case LeftShift(x, _) => List(x)
    case RightShift(x, _) => List(x)
    case BitwiseAnd(x, _) => List(x)

    case Print(s:S) => List(eax)
    case Allocate(n:S, init:S) => List(eax)

    case MemLoc(bp, _) => Nil
    case MemRead(loc) => Nil
    case MemWrite(loc, s:S) => Nil
    case Goto(s: S) => Nil
    case Comp(s1, op, s2) => Nil
    case CJump(comp, l1, l2) => Nil

    case Return => Nil // for now
  }
}

/**
 * From page 60 in lecture notes 4. Gen and kill with ret and call handling.
 *
 *                  gen           kill
 1:              ()            (ebx edi esi)
    :f
 2:              (edx)         (x2)
    (x2 <- edx)
 3:              (x2)          (x2)
    (x2 *= x2)
 4:              (x2)          (2x2)
    (2x2 <- x2)
 5:              (2x2)         (2x2)
    (2x2 *= 2)
 6:              (edx)         (3x)
    (3x <- edx)
 7:              (3x)          (3x)
    (3x *= 3)
 8:              (2x2)         (eax)
    (eax <- 2x2)
 9:              (3x eax)      (eax)
    (eax += 3x)
10:              (eax)         (eax)
    (eax += 4)
11:              (ebx edi esi) ()
    (return)

 */


/**
  case class Call(s:S) extends Instruction
  case class L2Function(name: LabelDeclaration, body: List[Instruction])
 */
