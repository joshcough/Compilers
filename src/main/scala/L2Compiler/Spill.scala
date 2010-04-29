package L2Compiler

import L2Compiler.L2AST._

trait Spill {
  def spill(spillVar:Variable, stackOffset: Int, spillPrefix:String, ins: List[Instruction]): List[Instruction] = {
    val memLoc = MemLoc(ebp, Num(stackOffset))
    val readSpillVar = MemRead(memLoc)
    def biop(x:X, s:S, default: Instruction, f: (X,S) => Instruction): List[Instruction] = {
      // (x += x)
      if(x == s && x == spillVar) {
        val newVar0 = Variable(spillPrefix + 0)
        List(Assignment(newVar0, memLoc),
            f(newVar0, newVar0),
            MemWrite(memLoc, newVar0))
      }
      // (x += something)
      else if(x == spillVar){
        val newVar0 = Variable(spillPrefix + 0)
        List(Assignment(newVar0, MemRead(memLoc)),
            f(newVar0, s),
            MemWrite(memLoc, newVar0))
      }
      // (y += x)
      else if(s == spillVar){
        val newVar0 = Variable(spillPrefix + 0)
        List(Assignment(newVar0, readSpillVar), f(x, newVar0))
      }
      else List(default)
    }
    def spill(i:Instruction): List[Instruction] = i match {
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
          // TODO what number to use for new var?
          val newVar0 = Variable(spillPrefix + 0)
          val newVar1 = Variable(spillPrefix + 1)
          List(Assignment(newVar0, readSpillVar),
              Assignment(newVar1, MemRead(MemLoc(newVar0, off))),
              MemWrite(memLoc, newVar1))
        }
        else if(v1 == spillVar) {
          // x <- (mem y n)
          // (s_0 <- (mem y n))
          // ((mem ebp stackOffset) <- s_0)
          val newVar0 = Variable(spillPrefix + 0)
          List(Assignment(newVar0, read),
              MemWrite(memLoc, newVar0))
        }
        else if(v2 == spillVar){
          // y <- (mem x n)
          // (s_0 <- (mem ebp stackOffset))
          // (y <- (mem s_0 n)
          // TODO what number to use for new var?
          val newVar0 = Variable(spillPrefix + 0)
          val newVar1 = Variable(spillPrefix + 1)
          List(Assignment(newVar0, readSpillVar),
              Assignment(v1, MemRead(MemLoc(newVar0, off))))
        }
        else List(ass)
      }
      // finally, instruction must be x <- register or x <- constant
      // if v is not spillVar, we just end up returning the entire thing. 
      case ass@Assignment(v:Variable, s:S) if(v == spillVar) =>
        List(MemWrite(memLoc, s))

      case mul@Multiply(x:X, s:S) => biop(x,s,mul,Multiply(_,_))
      case inc@Increment(x:X, s:S) => biop(x,s,inc,Increment(_,_))
      case dec@Decrement(x:X, s:S) => biop(x,s,dec,Decrement(_,_))
      case ls@LeftShift(x:X, s:S) => biop(x,s,ls,LeftShift(_,_))
      case rs@RightShift(x:X, s:S) => biop(x,s,rs,RightShift(_,_))
      case bwa@BitwiseAnd(x:X, s:S) => biop(x,s,bwa,BitwiseAnd(_,_))

      case _ => List(i)
    }
    ins.flatMap(spill)
  }
}