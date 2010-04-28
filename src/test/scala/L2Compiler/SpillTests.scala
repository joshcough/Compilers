package L2Compiler

import L2Compiler.L2AST._

class SpillTests extends L2CompilerTest {

  /**
     *(((x <- 1) (eax += x))) x -4 s
     *   (((mem ebp -4) <- 1)
     *  (s_0 <- (mem ebp -4))
     *  (eax += s_0))
     */
  testSpill("(((x <- 1) (eax += x)))",
    MemWrite(MemLoc(ebp,Num(-4)),Num(1)),
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(eax,Variable("s_0")))

  testSpill("(((x <- 7)))", MemWrite(MemLoc(ebp,Num(-4)),Num(7)))
  testSpill("(((x <- eax)))", MemWrite(MemLoc(ebp,Num(-4)),eax))

  testSpill("(((x <- (mem y 4))))",
    Assignment(Variable("s_0"),MemRead(MemLoc(Variable("y"),Num(4)))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_0")))

  testSpill("(((x <- (mem x 4))))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("s_1"),MemRead(MemLoc(Variable("s_0"),Num(4)))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))

  testSpill("(((y <- (mem x 4))))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("y"),MemRead(MemLoc(Variable("s_0"),Num(4)))))


  testSpill("(((eax += x)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(eax,Variable("s_0")))
  testSpill("(((x += eax)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(Variable("s_0"),eax),
    MemWrite(MemLoc(ebp,Num(-4)), Variable("s_0")))
  testSpill("(((x += y)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(Variable("s_0"),Variable("y")),
    MemWrite(MemLoc(ebp,Num(-4)), Variable("s_0")))

}
