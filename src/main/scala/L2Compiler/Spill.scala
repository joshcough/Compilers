package L2Compiler

import L2AST._

object SpillMain {
  import L2CompilerExtras._
  import io.FileHelper._

  def main(args:Array[String]){
    println(spill(new java.io.File(args(0)).read))
  }
  
  // % spill f.L2f
  //(((mem ebp -4) <- 1)
  // (s_0 <- (mem ebp -4))
  // (eax += s_0))
  def spill(input:String) = {
    resetSpillCounter()
    // this is an example input: ((x <- x)) x -4 s_
    // read the program part first, and then deal with the rest.
    val (program, rest) = readWithRest(input)
    // the test has these three things that we read.
    val List(varToSpill, offset, prefix) = read("(" + rest + ")").asInstanceOf[List[Any]]
    // after we have the program and the other arguments
    val newProgram = L2CompilerExtras.spill(
      Variable(varToSpill.toString.drop(1)), offset.toString.toInt,
      prefix.toString.drop(1), parseListOfInstructions(program.asInstanceOf[List[Any]]))
    newProgram.map(toCode).mkString("(", " ", ")")
  }
}

trait Spill {

  private var varCounter = Iterator.from(0)
  def resetSpillCounter(){ varCounter = Iterator.from(0) }

  def spill(spillVar:Variable, stackOffset: Int, ins: List[Instruction]): List[Instruction] =
    spill(spillVar, stackOffset, "spilled_var_", ins)

  def spill(spillVar:Variable, stackOffset: Int, spillPrefix:String, ins: List[Instruction]): List[Instruction] = {

    ///////////// Helper functions //////////////////
    val memLoc = MemLoc(ebp, Num(stackOffset))
    val readSpillVar = MemRead(memLoc)
    def newVar() = Variable(spillPrefix + varCounter.next())

    ///////////// Spill for Assignments //////////////////
    def spillAssignment(ass:Assignment): List[Instruction] = ass match {
      case Assignment(v1:Variable, v2:Variable) => {
        // if we have x <- x, just remove it.
        if(v1 == spillVar && v1 == v2) List()
        // x <- y where x is spillVar
        else if( v1 == spillVar ) List(MemWrite(memLoc, v2))
        // y <- x where x is spillVar
        else if( v2 == spillVar ) List(Assignment(v1, readSpillVar))
        // y <- z
        else List(ass)
      }
      case Assignment(v:Variable, s:S) => {
        if(v == spillVar) List(MemWrite(memLoc, s)) else List(ass)
      }
      case Assignment(r:Register, v2:Variable) => {
        if( v2 == spillVar ) List(Assignment(r, readSpillVar)) else List(ass)
      }
      case Assignment(r1:Register, r2:S) => List(ass)

      case Assignment(v1, read@MemRead(MemLoc(v2, off))) => {
        if(v1 == v2 && v1 == spillVar) {
          // funny case, x <- (mem x n) where x is spill var
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- (mem s_0 off)
          // ((mem ebp stackOffset) <- s_1)
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),
              Assignment(newVar0, MemRead(MemLoc(newVar0, off))),
              MemWrite(memLoc, newVar0))
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
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),
              Assignment(v1, MemRead(MemLoc(newVar0, off))))
        }
        else List(ass)
      }

      case Assignment(v, comp@Comp(x1, op, x2)) => {
        if( v == spillVar ){
          // (x <- x < x) ... wtf
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- s_0 < s_0)
          // ((mem ebp stackOffset) <- s_1)
          if(x1 == spillVar && x2 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar0, Comp(newVar0, op, newVar0)),
                MemWrite(memLoc, newVar0))
          }
          // (x <- x < y)
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- s_0 < y)
          // ((mem ebp stackOffset) <- s_1)
          else if(x1 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar0, Comp(newVar0, op, x2)),
                MemWrite(memLoc, newVar0))
          }
          // (x <- y < x)
          // (s_0 <- (mem ebp stackOffset))
          // (s_1 <- y < s_0)
          // ((mem ebp stackOffset) <- s_1)
          else if(x2 == spillVar){
            val newVar0 = newVar()
            List(Assignment(newVar0, readSpillVar),
                Assignment(newVar0, Comp(x1, op, newVar0)),
                MemWrite(memLoc, newVar0))
          }
          // (x <- y < z)
          // (s_0 <- y < z)
          // ((mem ebp stackOffset) <- s_0)
          else {
            val newVar0 = newVar()
            List(Assignment(newVar0, Comp(x1, op, x2)),
                MemWrite(memLoc, newVar0))
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

      // (eax <- (print s))
      case Assignment(eax, Print(s:S)) => {
        if(s == spillVar) {
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, Print(newVar0)))
        } else List(ass)
      }

      case Assignment(eax, Allocate(n, init)) => {
        //(eax <- (allocate x x))
        if(n == spillVar && init == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, Allocate(newVar0, newVar0)))
        }
        //(eax <- (allocate x i))
        else if(n == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, Allocate(newVar0, init)))
        }
        //(eax <- (allocate n x))
        else if(init == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, Allocate(n, newVar0)))
        }
        else List(ass)
      }

      case Assignment(eax, ArrayError(a, n)) => {
        //(eax <- (allocate x x))
        if(a == spillVar && n == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, ArrayError(newVar0, newVar0)))
        }
        //(eax <- (allocate x i))
        else if(a == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, ArrayError(newVar0, n)))
        }
        //(eax <- (allocate n x))
        else if(n == spillVar){
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar), Assignment(eax, ArrayError(a, newVar0)))
        }
        else List(ass)
      }
    }

    ///////////// Spill for Math Ops //////////////////
    def spillMathOp(x:X, s:S, default: Instruction, f: (X,S) => Instruction): List[Instruction] = {
      // (x += x)
      if(x == s && x == spillVar) {
        val newVar0 = newVar()
        List(Assignment(newVar0, MemRead(memLoc)),
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

    ///////////////////////// spill /////////////////////////////////
    def spill(i:Instruction): List[Instruction] = i match {

      case ass:Assignment => spillAssignment(ass)

      ///////////// MathOp //////////////////
      case mul@Multiply(x:X, s:S) => spillMathOp(x,s,mul,Multiply(_,_))
      case inc@Increment(x:X, s:S) => spillMathOp(x,s,inc,Increment(_,_))
      case dec@Decrement(x:X, s:S) => spillMathOp(x,s,dec,Decrement(_,_))
      case bwa@BitwiseAnd(x:X, s:S) => spillMathOp(x,s,bwa,BitwiseAnd(_,_))
      case ls@LeftShift(x:X, s:S) => spillMathOp(x,s,ls,LeftShift(_,_))
      case rs@RightShift(x:X, s:S) => spillMathOp(x,s,rs,RightShift(_,_))

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
          List(Assignment(newVar0, readSpillVar),MemWrite(MemLoc(base, off), newVar0))
        }
        else List(mw)
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
      case c@Call(s) => {
        if(s==spillVar) {
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),Call(newVar0))
        }
        else List(c)
      }
      case tc@TailCall(s) => {
        if(s==spillVar) {
          val newVar0 = newVar()
          List(Assignment(newVar0, readSpillVar),TailCall(newVar0))
        }
        else List(tc)
      }
      case g:Goto => List(g)
      case ld:LabelDeclaration => List(ld)
      case Return => List(Return)
    }
    // finally, call spill for each instruction.
    ins.flatMap(spill)
  }
}