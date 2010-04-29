package L2Compiler

import L2Compiler.L2AST._

class SpillTests extends L2CompilerTest {

  def testSpill(code:String, expected: Instruction*) = test(code){
    val newProgramList =
      compiler.spill(Variable("x"), -4, "s_", compiler.parseInstructionListThing(code))
    //println(newProgram)
    assert(newProgramList === expected.toList)
  }

  /**
     *(((x <- 1) (eax += x))) x -4 s
     *   (((mem ebp -4) <- 1)
     *  (s_0 <- (mem ebp -4))
     *  (eax += s_0))
     */
  testSpill("((x <- 1) (eax += x))",
    MemWrite(MemLoc(ebp,Num(-4)),Num(1)),
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(eax,Variable("s_0")))

  testSpill("((x <- 7))", MemWrite(MemLoc(ebp,Num(-4)),Num(7)))
  testSpill("((x <- eax))", MemWrite(MemLoc(ebp,Num(-4)),eax))

  testSpill("((x <- (mem y 4)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(Variable("y"),Num(4)))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_0")))

  testSpill("((x <- (mem x 4)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("s_1"),MemRead(MemLoc(Variable("s_0"),Num(4)))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))

  testSpill("((y <- (mem x 4)))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("y"),MemRead(MemLoc(Variable("s_0"),Num(4)))))

  testSpill("((x <- x < x))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("s_1"),Comp(Variable("s_0"),LessThan,Variable("s_0"))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))
  
  testSpill("((x <- x < y))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("s_1"),Comp(Variable("s_0"),LessThan,Variable("y"))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))

  testSpill("((x <- y < x))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("s_1"),Comp(Variable("y"),LessThan,Variable("s_0"))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))
  
  testSpill("((x <- y < z))",
    Assignment(Variable("s_0"),Comp(Variable("y"),LessThan,Variable("z"))),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))
  
  testSpill("((y <- x < x))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("y"),Comp(Variable("s_0"),LessThan,Variable("s_0"))))

  testSpill("((y <- x < z))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("y"),Comp(Variable("s_0"),LessThan,Variable("z"))))
  
  testSpill("((y <- z < x))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Assignment(Variable("y"),Comp(Variable("z"),LessThan,Variable("s_0"))))

  testSpill("((y <- z1 < z2))", Assignment(Variable("y"),Comp(Variable("z1"),LessThan,Variable("z2"))))  

  testSpill("((x <- eax < ebx)))",
    Assignment(Variable("s_0"),Comp(eax,LessThan,ebx)),
    MemWrite(MemLoc(ebp,Num(-4)),Variable("s_1")))

  testSpill("((eax += x))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(eax,Variable("s_0")))
  testSpill("((x += eax))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(Variable("s_0"),eax),
    MemWrite(MemLoc(ebp,Num(-4)), Variable("s_0")))
  testSpill("((x += y))",
    Assignment(Variable("s_0"),MemRead(MemLoc(ebp,Num(-4)))),
    Increment(Variable("s_0"),Variable("y")),
    MemWrite(MemLoc(ebp,Num(-4)), Variable("s_0")))

}
