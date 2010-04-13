package L1Compiler

import reader._
import L1AST._

class L1ParseTests extends ParsePrimitivesTest with
        ParseAssignmentsTest with ParseMathTest with
        ParseCJumpTest with ParseOtherStuffTest with ParseProgramsTest

trait ParseProgramsTest extends L1ParserTest{
  testParse("(((eax <- 7)))" ->
          L1(L1Function(Label("main"),
             List(RegisterAssignment(eax, Num(7)))))
  )

  testParse("(((eax <- 7) (ebx <- 8)))" ->
          L1(L1Function(Label("main"),
             List(RegisterAssignment(eax, Num(7)),
               RegisterAssignment(ebx, Num(8)))))
  )

  testParse("(((eax <- 7)) (:fun2 (ecx <- 8)))" ->
          L1(L1Function(Label("main"),
             List(RegisterAssignment(eax, Num(7)))),
            List(L1Function(Label("fun2"),
             List(RegisterAssignment(ecx, Num(8))))))
  )
  
  testParse("(((eax <- 7)) (:fun2 (ecx <- 8)) (:fun3 (edx <- 10)))" ->
          L1(L1Function(Label("main"),
             List(RegisterAssignment(eax, Num(7)))),
            List(
              L1Function(Label("fun2"), List(RegisterAssignment(ecx, Num(8)))),
              L1Function(Label("fun3"), List(RegisterAssignment(edx, Num(10))))))
  )
}

trait ParsePrimitivesTest extends L1ParserTest {
  testParseInstruction("eax" -> eax)
  testParseInstruction("ebx" -> ebx)
  testParseInstruction("ecx" -> ecx)
  testParseInstruction("edx" -> edx)
  testParseInstruction("esi" -> esi)
  testParseInstruction("edi" -> edi)
  testParseInstruction("ebp" -> ebp)
  testParseInstruction("esp" -> esp)
  testParseInstruction(":hey" -> Label("hey"))
  testParseInstruction("1" -> Num(1))
  testParseInstructionError("eex" -> "eex is an invalid register")
}

trait ParseAssignmentsTest extends L1ParserTest {
  // simple register writes
  testParseInstruction("(eax <- 7)" -> RegisterAssignment(eax, Num(7)))
  testParseInstruction("(eax <- ebx)" -> RegisterAssignment(eax, ebx))
  testParseInstruction("(ebx <- eax)" -> RegisterAssignment(ebx, eax))

  // mem read
  testParseInstruction("(eax <- (mem eax 4))" ->
          RegisterAssignment(eax, MemRead(MemLoc(eax, Num(4)))))
  testParseInstruction("(eax <- (mem ebx 4))" ->
          RegisterAssignment(eax, MemRead(MemLoc(ebx, Num(4)))))

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
  testParseInstruction("(ebx <- 1 < 2)" -> RegisterAssignment(ebx, Comp(Num(1), LessThan, Num(2))))
  testParseInstruction("(ebx <- ebx < 2)" -> RegisterAssignment(ebx, Comp(ebx, LessThan, Num(2))))
  testParseInstruction("(ebx <- eax < 2)" -> RegisterAssignment(ebx, Comp(eax, LessThan, Num(2))))
  testParseInstruction("(ebx <- eax < edx)" -> RegisterAssignment(ebx, Comp(eax, LessThan, edx)))
  testParseInstructionError("(ebx <- eax < eex)" -> "eex is an invalid register")
}

trait ParseMathTest extends L1ParserTest {
  testParseInstruction("(eax += 7)" -> Increment(eax, Num(7)))
  testParseInstruction("(eax -= ebx)" -> Decrement(eax, ebx))
  testParseInstruction("(ebx *= eax)" -> Multiply(ebx, eax))
}

trait ParseCJumpTest extends L1ParserTest {
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

trait ParseOtherStuffTest extends L1ParserTest {
  testParseInstruction("(goto ebx)" -> Goto(ebx))
  testParseInstruction("(goto :label)" -> Goto(Label("label")))
  testParseInstruction("(goto 7)" -> Goto(Num(7)))
  testParseInstruction("(call :func)" -> Call(Label("func")))
  testParseInstruction("(call eax)" -> Call(eax))
  testParseInstruction("(return)" -> Return)

//  testParseSExpr(
//    (List(
//      List(
//        List('esi, '<-, 7),
//        List('edi, '<-, 7),
//        List('esi, '+=, 'edi),
//        List('esi, '-=, 1),
//        List('eax, '<-, List('print, 'esi))
//        )
//      ) -> L1(L1Function(Label("main"),
//             List(RegisterAssignment(eax, Num(7)))))))  
}

abstract class L1ParserTest extends org.scalatest.FunSuite{
  val parser = L1Parser
  def read(s:String): Any = new Reader().read(s)
  def parseInstruction(a:Any) = parser parseInstruction a
  def parse(a:Any) = parser parse a

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
