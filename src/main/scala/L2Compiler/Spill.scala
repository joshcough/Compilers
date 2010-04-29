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

      // (eax <- (print x))
      case Print(s:S) if s == spillVar => {
        val newVar0 = newVar()
        List(Assignment(newVar0, readSpillVar),Print(newVar0))
      }
      case Goto(s:S) if s == spillVar => {
        val newVar0 = newVar()
        List(Assignment(newVar0, readSpillVar),Goto(newVar0))
      }
      case Call(s:S) if s == spillVar => {
        val newVar0 = newVar()
        List(Assignment(newVar0, readSpillVar),Call(newVar0))
      }
      case mw@MemWrite(loc@MemLoc(base, off), s) => {
        // ((mem x 4) <- x)
        // (s_0 <- (mem ebp stackOffset))
        // ((mem s_0 4) <- s_0)
        if(base == spillVar && s == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),MemWrite(MemLoc(newVar0, off), newVar0))
        }
        // ((mem x 4) <- y)
        // (s_0 <- (mem ebp stackOffset))
        // ((mem s_0 4) <- y)
        else if(base == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),MemWrite(MemLoc(newVar0, off), s))
        }
        // ((mem y 4) <- x)
        // (s_0 <- (mem ebp stackOffset))
        // ((mem y 4) <- s_0)
        else if(s == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),MemWrite(MemLoc(newVar0, off), s))
        }
        else List(mw)
      }
      case a@Allocate(n, init) => {
        //(eax <- (allocate x x))
        if(n == spillVar && init == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),Allocate(newVar0, newVar0))
        }
        //(eax <- (allocate x i))
        else if(n == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),Allocate(newVar0, init))
        }
        //(eax <- (allocate n x))
        else if(init == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),Allocate(n, newVar0))
        }
        else List(a)
      }
      case cj@CJump(Comp(s1, op, s2), l1, l2) => {
        // (cjump x < x :l1 :l2)
        if(s1 == spillVar && s2 == spillVar) {
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), CJump(Comp(newVar0, op, newVar0), l1, l2))
        }
        // (cjump x < y  :l1 :l2)
        else if(s1 == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), CJump(Comp(newVar0, op, s2), l1, l2))
        }
        // (cjump y < x :l1 :l2)
        else if(s2 == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), CJump(Comp(s1, op, newVar0), l1, l2))
        }
        else List(cj)
      }
      case _ => List(i)
    }
    ins.flatMap(spill)
  }
}