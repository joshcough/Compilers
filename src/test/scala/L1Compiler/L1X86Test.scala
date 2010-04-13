package L1Compiler

import reader._
import L1AST._
import FileHelper._
import java.io.File

class GeneratorTests extends GenMathInstructionsTest with RegisterAssigmentInstructionsTest with GenFullProgramTest

trait GenFullProgramTest extends L1X86Test {
  testCompileString("(((eax <- 5)(eax <- (print eax))))" -> "2")
  testCompileString("(((eax <- (allocate 3 3))(eax <- (print eax))))" -> "{s:1, 1}")
}

trait GenMathInstructionsTest extends L1X86Test {
  testInstructionGen("(eax += 7)" -> "addl $7, %eax")
  testInstructionGen("(eax += ecx)" -> "addl %ecx, %eax")
  testInstructionGen("(eax -= 7)" -> "subl $7, %eax")
  testInstructionGen("(eax -= ecx)" -> "subl %ecx, %eax")
}


trait RegisterAssigmentInstructionsTest extends L1X86Test {
  testInstructionGen("(eax <- 7)" -> "movl $7, %eax")
}

trait L1X86Test extends org.scalatest.FunSuite{
  val parser = L1Parser
  def read(s:String): Any = new Reader().read(s)
  def parseInstruction(a:Any) = parser parseInstruction a
  def parse(a:Any) = parser parse a
  def generateCodeForInstruction(i:Instruction) = L1X86Generator.generateCode(i)
  def generateCode(program:L1) = L1X86Generator.generateCode(program)

  def testInstructionGen(t: (String, String)): Unit = {
    test(t._1 + " => " + t._2){
      assert(generateCodeForInstruction(parseInstruction(read(t._1))) === t._2)
    }
  }

  def testCompileFile(t: (String, String)): Unit = {
    testCompile(t._1, t._2, new File(t._1).read)
  }

  def testCompileString(t: (String, String)): Unit = {
    testCompile(t._1, t._2, t._1)
  }  

  private def testCompile(testName: String, expectedResults: String, stringToRead: String): Unit = {

    def runAndDieOneErrors(cmd:String): String = {
      val (out, err) = CommandRunner(cmd)
      if(err != "") error("[" + cmd + "] died with the following errors:\n" + err) else out
    }

    test(testName + " => " + expectedResults){
      new File("/tmp/test.S").write(generateCode(parse(read(stringToRead))))
      runAndDieOneErrors("gcc -O2 -c -o /tmp/runtime.o ./src/main/compilers/L1/runtime.c")
      runAndDieOneErrors("as -o /tmp/test.o /tmp/test.S")
      runAndDieOneErrors("gcc -o /tmp/a.out /tmp/test.o /tmp/runtime.o")
      assert(runAndDieOneErrors("/tmp/a.out") === expectedResults)
    }
  }
}
