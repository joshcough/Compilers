package L2Compiler

import L2Compiler.L2AST._

trait Spill {
  def spill(spillVar:Variable, stackOffset: Int, spillPrefix:String, ins: List[Instruction]): List[Instruction] = {
    //////////////////////////////////////////////////////////
    // Helper functions
    //////////////////////////////////////////////////////////
    val memLoc = MemLoc(ebp, Num(stackOffset))
    val readSpillVar = MemRead(memLoc)
    var newVarCount = -1
    def newVar() = {
      newVarCount += 1
      Variable(spillPrefix + newVarCount)
    }
    def biop(x:X, s:S, default: Instruction, f: (X,S) => Instruction): List[Instruction] = {
      // (x += x)
      if(x == s && x == spillVar) {
        val newVar0 = newVar()
        List(Assignment(newVar0, memLoc),
            f(newVar0, newVar0),
            MemWrite(memLoc, newVar0))
      }
      // (x += something)
      else if(x == spillVar){
        val newVar0 = newVar()
        List(Assignment(newVar0, MemRead(memLoc)),
            f(newVar0, s),
            MemWrite(memLoc, newVar0))
      }
      // (y += x)
      else if(s == spillVar){
        val newVar0 = newVar()
        List(Assignment(newVar0, readSpillVar), f(x, newVar0))
      }
      else List(default)
    }
    //////////////////////////////////////////////////////////
    // The meat!
    //////////////////////////////////////////////////////////
    def spill(i:Instruction): List[Instruction] = i match {
      ///////////// Assignments //////////////////
      case ass@Assignment(v1:Variable, v2:Variable) => {
        // if we have x <- x, just leave it...handle it in L1 compiler. dumb.
        if(v1 == v2) List(ass)
        // x <- y where x is spillVar
        else if( v1 == spillVar ) List(MemWrite(memLoc, v2))
        // y <- x where x is spillVar
        else if( v2 == spillVar ) List(Assignment(v1, readSpillVar))
        // y <- z
        else List(ass)
      }
      case ass@Assignment(v1:Variable, read@MemRead(MemLoc(v2:Variable, off))) => {
        if(v1 == v2 && v1 == spillVar) {
          // funny case, x <- (mem x n) where x is spill var
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- (mem s_0 off)
          // ((mem ebp stackOffset) <- s_1)
          val newVar0 = newVar()
          val newVar1 = newVar()
          List(Assignment(newVar0, readSpillVar),
              Assignment(newVar1, MemRead(MemLoc(newVar0, off))),
              MemWrite(memLoc, newVar1))
        }
        else if(v1 == spillVar) {
          // x <- (mem y n)
          // (s_0 <- (mem y n))
          // ((mem ebp stackOffset) <- s_0)
          val newVar0 = newVar()
          List(Assignment(newVar0, read),
              MemWrite(memLoc, newVar0))
        }
        else if(v2 == spillVar){
          // y <- (mem x n)
          // (s_0 <- (mem ebp stackOffset))
          // (y <- (mem s_0 n)
          // TODO what number to use for new var?
          val newVar0 = newVar()
          val newVar1 = newVar()
          List(Assignment(newVar0, readSpillVar),
              Assignment(v1, MemRead(MemLoc(newVar0, off))))
        }
        else List(ass)
      }
      case ass@Assignment(v:Variable, comp@Comp(x1, op, x2)) => {
        if( v == spillVar ){
          // (x <- x < x) ... wtf
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- s_0 < s_0)
          // ((mem ebp stackOffset) <- s_1)
          if(x1 == spillVar && x2 == spillVar){
            val newVar0 = newVar()
            val newVar1 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar1, Comp(newVar0, op, newVar0)),
                MemWrite(memLoc, newVar1))
          }
          // (x <- x < y)
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- s_0 < y)
          // ((mem ebp stackOffset) <- s_1)
          else if(x1 == spillVar){
            val newVar0 = newVar()
            val newVar1 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar1, Comp(newVar0, op, x2)),
                MemWrite(memLoc, newVar1))
          }
          // (x <- y < x)
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- y < s_0)
          // ((mem ebp stackOffset) <- s_1)
          else if(x2 == spillVar){
            val newVar0 = newVar()
            val newVar1 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar1, Comp(x1, op, newVar0)),
                MemWrite(memLoc, newVar1))
          }
          // (x <- y < z)
          // (s_0 <- y < z)
          // ((mem ebp stackOffset) <- s_0)
          else {
            val newVar0 = newVar()
            val newVar1 = newVar()
            List(Assignment(newVar0, Comp(x1, op, x2)),
                MemWrite(memLoc, newVar1))
          }
        }
        // else v is not the spill var...
        else {
          // (y <- x < x)
          // (s_0 <- (mem ebp stackOffset))
          // (y <- s_0 < s_0)
          if(x1 == spillVar && x2 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(v, Comp(newVar0, op, newVar0)))
          }
          // (y <- x < z)
          // (s_0 <- (mem ebp stackOffset))
          // (y <- s_0 < z)
          else if(x1 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(v, Comp(newVar0, op, x2)))
          }
          // (y <- z < x)
          // (s_0 <- (mem ebp stackOffset))
          // (y <- z < s_0)
          else if(x2 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(v, Comp(x1, op, newVar0)))
          }
          else List(ass)
        }
      }

      // finally, instruction must be x <- register or x <- constant
      // if v is not spillVar, we just end up returning the entire thing. 
      case ass@Assignment(v:Variable, s:S) if(v == spillVar) =>
        List(MemWrite(memLoc, s))

      ///////////// Biop //////////////////
      case mul@Multiply(x:X, s:S) => biop(x,s,mul,Multiply(_,_))
      case inc@Increment(x:X, s:S) => biop(x,s,inc,Increment(_,_))
      case dec@Decrement(x:X, s:S) => biop(x,s,dec,Decrement(_,_))
      case ls@LeftShift(x:X, s:S) => biop(x,s,ls,LeftShift(_,_))
      case rs@RightShift(x:X, s:S) => biop(x,s,rs,RightShift(_,_))
      case bwa@BitwiseAnd(x:X, s:S) => biop(x,s,bwa,BitwiseAnd(_,_))

      //case _ => List(i)
    }
    ins.flatMap(spill)
  }
}

/**
 * package L2Compiler

object L2AST {

  object L2{ def apply(main: L2Function): L2 = L2(main, Nil) }
  case class L2(main: L2Function, funs:List[L2Function])

  sealed trait Instruction{
    def toL2Code: String
  }
  sealed trait S extends Instruction
  case class Num(n: Int) extends S {
    def toL2Code: String = n.toString
  }
  case class Label(l: String) extends S {
    def toL2Code: String = ":" + l
  }
  case class LabelDeclaration(l: Label) extends Instruction {
    def toL2Code: String = l.toL2Code
  }
  sealed trait X extends S
  case class Variable(val name: String) extends X {
    def toL2Code: String = name
  }
  abstract class Register(val name: String) extends X {
    def toL2Code: String = name
  }
  case class Comp(s1: S, op: CompOp, s2: S) extends Instruction {
    def toL2Code: String = s1.toL2Code + " " + op.op + " " + s2.toL2Code
  }
  sealed abstract case class CompOp(op: String){
    def apply(x:Int, y:Int): Boolean
  }
  object LessThan extends CompOp("<"){
    def apply(x:Int, y:Int) = x < y
  }
  object LessThanOrEqualTo extends CompOp("<="){
    def apply(x:Int, y:Int) = x <= y
  }
  object EqualTo extends CompOp("="){
    def apply(x:Int, y:Int) = x == y
  }

  case class Allocate(n:S, init: S) extends Instruction {
    def toL2Code: String = "(allocate " + n.toL2Code + " " + init.toL2Code + ")"
  }
  case class Assignment(x: X, s: Instruction) extends Instruction {
    def toL2Code: String = "(" + x.toL2Code + " <- " + s.toL2Code + ")"
  }
  case class MemLoc(basePointer: X, offset: Num) extends Instruction {
    def toL2Code: String = "(mem " + basePointer.toL2Code + " " + offset.toL2Code + ")"
  }
  case class MemRead(loc: MemLoc) extends Instruction {
    def toL2Code: String = loc.toL2Code
  }
  case class MemWrite(loc: MemLoc, e: S) extends Instruction {
    def toL2Code: String = "(" + loc.toL2Code + " <- " + e.toL2Code + ")"
  }
  case class Print(e: S) extends Instruction {
    def toL2Code: String = "(print " + e.toL2Code + ")"
  }
  // TODO: check if interpreter allows (goto num) and (goto register)
  case class Goto(s: S) extends Instruction {
    def toL2Code: String = "(goto " + s.toL2Code + ")"
  }
  case class CJump(comp:Comp, l1: Label, l2: Label) extends Instruction {
    def toL2Code: String = "(cjump " + comp.toL2Code + " " + l1.toL2Code  + " " + l2.toL2Code + ")"
  }
  case class Call(s:S) extends Instruction {
    def toL2Code: String = "(call " + s.toL2Code + ")"
  }
  case object Return extends Instruction {
    def toL2Code: String = "(return)"
  }
  case class L2Function(name: LabelDeclaration, body: List[Instruction])
}

 */
