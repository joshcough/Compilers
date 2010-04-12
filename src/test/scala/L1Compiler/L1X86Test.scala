package L1Compiler

import reader._
import L1AST._

class GeneratorTests extends GenMathInstructionsTest

trait GenMathInstructionsTest extends L1X86Test {
  testInstructionGen("(eax += 7)" -> "addl $7, %eax")
  testInstructionGen("(eax += ecx)" -> "addl %ecx, %eax")
}

trait L1X86Test extends org.scalatest.FunSuite{
  val parser = L1Parser
  def read(s:String): Any = new Reader().read(s)
  def parseInstruction(a:Any) = parser parseInstruction a
  //def parse(a:Any) = parser parse a
  def generateCodeForInstruction(i:Instruction) = L1X86Generator.generateCode(i)

  def testInstructionGen(t: (String, String)): Unit = {
    test(t._1 + " => " + t._2){
      assert(generateCodeForInstruction(parseInstruction(read(t._1))) === t._2)
    }
  }
}
