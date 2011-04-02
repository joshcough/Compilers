package L2Compiler
/**
import L2AST._
import L1Compiler.L1AST._

class ParsePrimitivesTest extends L2CompilerTest {
  testParseInstruction("eex" -> Variable("eex"))
}

class ParseAssignmentsTest extends L2CompilerTest {
  testParseInstruction("(eax <- (allocate eex 1))" -> Allocate(Variable("eex"), Num(1)))
  testParseInstruction("(ebx <- eax < eex)" -> Assignment(ebx, Comp(eax, LessThan, Variable("eex"))))
}

class ParseCJumpTest extends L2CompilerTest {
  testParseInstruction("(cjump 2 <= efx :true :false)" ->
          CJump(Comp(Num(2), LessThanOrEqualTo, Variable("efx")), Label("true"), Label("false")))
}
**/