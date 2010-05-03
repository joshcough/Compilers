package L2Compiler

import L2AST._
import L1Compiler.L1AST._


//((x <- 1) (eax += x)) x -4 s
abstract class L2CompilerTest extends org.scalatest.FunSuite{

  val compiler = new L2Compiler{
    def generateCode(ast:L2):L1 = error("TODO")
  }
  import compiler._

  def testParseSExpr(t: (Any, L2)){
    test(t._1 + " => " + t._2){ assert(parse(t._1) === t._2) }
  }

  def testParse(t: (String, L2)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseProgram(t._1) === t._2) }
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