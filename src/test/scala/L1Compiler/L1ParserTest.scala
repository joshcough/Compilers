package L1Compiler

import L1AST._

class ParseProgramsTest extends L1ParserTest{
  testParse("(((eax <- 7)))" ->
          L1(Func(LabelDeclaration(Label("main")),
             List(Assignment(eax, Num(7)))))
  )

  testParse("(((eax <- 7) (ebx <- 8)))" ->
          L1(Func(LabelDeclaration(Label("main")),
             List(Assignment(eax, Num(7)),
               Assignment(ebx, Num(8)))))
  )

  testParse("(((eax <- 7)) (:fun2 (ecx <- 8)))" ->
          L1(Func(LabelDeclaration(Label("main")),
             List(Assignment(eax, Num(7)))),
            List(Func(LabelDeclaration(Label("fun2")),
             List(Assignment(ecx, Num(8))))))
  )

  testParse("(((eax <- 7)) (:fun2 (ecx <- 8)) (:fun3 (edx <- 10)))" ->
          L1(Func(LabelDeclaration(Label("main")),
             List(Assignment(eax, Num(7)))),
            List(
              Func(LabelDeclaration(Label("fun2")), List(Assignment(ecx, Num(8)))),
              Func(LabelDeclaration(Label("fun3")), List(Assignment(edx, Num(10))))))
  )
  testParse("""((
  :aint_gonna_happen
  :terminate))""" ->
          L1(Func(LabelDeclaration(Label("main")),
             List(LabelDeclaration(Label("aint_gonna_happen")),
                  LabelDeclaration(Label("terminate")))))
  )
}

class ParsePrimitivesTest extends L1ParserTest {
  testParseInstruction("eax" -> eax)
  testParseInstruction("ebx" -> ebx)
  testParseInstruction("ecx" -> ecx)
  testParseInstruction("edx" -> edx)
  testParseInstruction("esi" -> esi)
  testParseInstruction("edi" -> edi)
  testParseInstruction("ebp" -> ebp)
  testParseInstruction("esp" -> esp)
  testParseInstruction(":hey" -> LabelDeclaration(Label("hey")))
  testParseInstruction("1" -> Num(1))
  testParseInstructionError("eex" -> "eex is an invalid register")
}

class ParseAssignmentsTest extends L1ParserTest {
  // simple register writes
  testParseInstruction("(eax <- 7)" -> Assignment(eax, Num(7)))
  testParseInstruction("(eax <- ebx)" -> Assignment(eax, ebx))
  testParseInstruction("(ebx <- eax)" -> Assignment(ebx, eax))

  // mem read
  testParseInstruction("(eax <- (mem eax 4))" ->
          Assignment(eax, MemRead(MemLoc(eax, Num(4)))))
  testParseInstruction("(eax <- (mem ebx 4))" ->
          Assignment(eax, MemRead(MemLoc(ebx, Num(4)))))

  // mem write
  testParseInstruction("((mem ebx 4) <- 7)" -> MemWrite(MemLoc(ebx, Num(4)), Num(7)))
  testParseInstruction("((mem ebx 4) <- eax)" -> MemWrite(MemLoc(ebx, Num(4)), eax))
  testParseInstruction("((mem ebx 4) <- esi)" -> MemWrite(MemLoc(ebx, Num(4)), esi))

  // print
  testParseInstruction("(eax <- (print eax))" -> Print(eax))
  testParseInstruction("(eax <- (print ebx))" -> Print(ebx))
  // TODO: figure out if this is even legal
  testParseInstruction("(eax <- (print 7))" -> Print(Num(7)))

  // allocate
  testParseInstruction("(eax <- (allocate eax 1))" -> Allocate(eax, Num(1)))
  testParseInstruction("(eax <- (allocate ebx 1))" -> Allocate(ebx, Num(1)))
  testParseInstructionError("(eax <- (allocate eex 1))" -> "eex is an invalid register")

  // cmp
  //(cx <- s cmp s)
  testParseInstruction("(ebx <- 1 < 2)" -> Assignment(ebx, Comp(Num(1), LessThan, Num(2))))
  testParseInstruction("(ebx <- ebx < 2)" -> Assignment(ebx, Comp(ebx, LessThan, Num(2))))
  testParseInstruction("(ebx <- eax < 2)" -> Assignment(ebx, Comp(eax, LessThan, Num(2))))
  testParseInstruction("(ebx <- eax < edx)" -> Assignment(ebx, Comp(eax, LessThan, edx)))
  testParseInstructionError("(ebx <- eax < eex)" -> "eex is an invalid register")
}

class ParseMathTest extends L1ParserTest {
  testParseInstruction("(eax += 7)" -> Increment(eax, Num(7)))
  testParseInstruction("(eax -= ebx)" -> Decrement(eax, ebx))
  testParseInstruction("(ebx *= eax)" -> Multiply(ebx, eax))

  testParseInstruction("(ebx >>= eax)" -> RightShift(ebx, eax))
  testParseInstruction("(ecx <<= eax)" -> LeftShift(ecx, eax))
  testParseInstruction("(edx &= eax)" -> BitwiseAnd(edx, eax))

  testParseInstruction("(ebx >>= 7)" -> RightShift(ebx, Num(7)))
  testParseInstruction("(ecx <<= 8)" -> LeftShift(ecx, Num(8)))
  testParseInstruction("(edx &= 9)" -> BitwiseAnd(edx, Num(9)))
}

class ParseCJumpTest extends L1ParserTest {
  testParseInstruction("(cjump eax = eax :true :false)" ->
          CJump(Comp(eax, EqualTo, eax), Label("true"), Label("false")))
  testParseInstruction("(cjump eax < ebx :true :false)" ->
          CJump(Comp(eax, LessThan, ebx), Label("true"), Label("false")))
  testParseInstruction("(cjump ebx <= eax :true :false)" ->
          CJump(Comp(ebx, LessThanOrEqualTo, eax), Label("true"), Label("false")))
  testParseInstruction("(cjump 2 <= 1 :true :false)" ->
          CJump(Comp(Num(2), LessThanOrEqualTo, Num(1)), Label("true"), Label("false")))
  testParseInstruction("(cjump 2 <= ebx :true :false)" ->
          CJump(Comp(Num(2), LessThanOrEqualTo, ebx), Label("true"), Label("false")))
  testParseInstructionError("(cjump 2 <= efx :true :false)" -> "efx is an invalid register")
}

class ParseOtherStuffTest extends L1ParserTest {
  testParseInstruction("(goto ebx)" -> Goto(ebx))
  testParseInstruction("(goto :label)" -> Goto(Label("label")))
  testParseInstruction("(call :func)" -> Call(Label("func")))
  testParseInstruction("(call eax)" -> Call(eax))
  testParseInstruction("(return)" -> Return)
  testParseInstructionError("(goto 7)" -> "unexpected token: List('goto, 7)")
}

abstract class L1ParserTest extends org.scalatest.FunSuite{
  val compiler = new L1Compiler with X86.L1X86Generator
  import compiler._

  def testParseSExpr(t: (Any, L1)){
    test(t._1 + " => " + t._2){ assert(parse(t._1) === t._2) }
  }

  def testParse(t: (String, L1)): Unit = {
    test(t._1 + " => " + t._2){ assert(parse(read(t._1)) === t._2) }
  }

  def testParseInstruction(t: (String, Instruction)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseInstruction(read(t._1)) === t._2) }
  }

  def testParseInstructionError(t: (String, String)): Unit  = {
    test(t._1 + " => " + t._2){
      val ex = intercept[Exception] { parseInstruction(read(t._1)) }
      assert(ex.getMessage === t._2)
    }
  }
}
