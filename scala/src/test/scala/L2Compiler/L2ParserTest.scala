package L2Compiler

import L2AST._

class L2ParserTest extends L2CompilerTest {
  testParseS('eex -> Variable("eex"))

  testParseInstruction("(eax <- (allocate eex 1))" -> Assignment(eax, Allocate(Variable("eex"), Num(1))))
  testParseInstruction("(ebx <- eax < eex)" -> Assignment(ebx, Comp(eax, LessThan, Variable("eex"))))

  testParseInstruction("(cjump 2 <= efx :true :false)" ->
    CJump(Comp(Num(2), LessThanOrEqualTo, Variable("efx")), Label("true"), Label("false")))

  def testParseS(t: (Any, S)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseS(t._1) === t._2) }
  }

  def testParseInstruction(t: (String, Instruction)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseInstruction(read(t._1)) === t._2) }
  }
}

