package L1Compiler.X86

import L1Compiler._
import L1Compiler.FileHelper._
import java.io.File

class TestCompilerVsInterpreter extends L1X86Test{
  //Dir.testFiles.filter(_.contains("labels-on-heap.L1")).foreach(testCompilerVsInterpreter)
  Dir.testFiles.foreach(testCompilerVsInterpreter)
}

class GenFullProgramTest extends L1X86Test {
  testCompileString("(((eax <- 5)(eax <- (print eax))))" -> "2")
  testCompileString("(((eax <- (allocate 3 3))(eax <- (print eax))))" -> "{s:1, 1}")
}

class GenMathInstructionsTest extends L1X86Test {
  testInstructionGen("(eax += 7)" -> List("addl $7, %eax"))
  testInstructionGen("(eax += ecx)" -> List("addl %ecx, %eax"))
  testInstructionGen("(eax -= 7)" -> List("subl $7, %eax"))
  testInstructionGen("(eax -= ecx)" -> List("subl %ecx, %eax"))
}

class RegisterAssigmentInstructionsTest extends L1X86Test {
  testInstructionGen("(eax <- 7)" -> List("movl $7, %eax"))
}

class JmpInstructionsTest extends L1X86Test {
  testInstructionGen("(eax <- :some_label)" -> List("movl $L1_some_label, %eax"))
  testInstructionGen("(goto :some_label)" -> List("jmp L1_some_label"))
  testInstructionGen("(goto eax)" -> List("jmp *%eax"))
}

class ComparisonInstructionsTest extends L1X86Test {
  testInstructionGen("(eax <- esi < edi)" ->
          List("cmp %edi, %esi", "setl %al", "movzbl %al, %eax"))  
  testInstructionGen("(ebx <- esi < edi)" ->
          List("cmp %edi, %esi", "setl %bl", "movzbl %bl, %ebx"))
  testInstructionGen("(ecx <- esi < edi)" ->
          List("cmp %edi, %esi", "setl %cl", "movzbl %cl, %ecx"))
  testInstructionGen("(edx <- esi < edi)" ->
          List("cmp %edi, %esi", "setl %dl", "movzbl %dl, %edx"))

  testInstructionGen("(eax <- esi <= edi)" ->
          List("cmp %edi, %esi", "setle %al", "movzbl %al, %eax"))
  testInstructionGen("(ebx <- esi <= edi)" ->
          List("cmp %edi, %esi", "setle %bl", "movzbl %bl, %ebx"))
  testInstructionGen("(ecx <- esi <= edi)" ->
          List("cmp %edi, %esi", "setle %cl", "movzbl %cl, %ecx"))
  testInstructionGen("(edx <- esi <= edi)" ->
          List("cmp %edi, %esi", "setle %dl", "movzbl %dl, %edx"))

  testInstructionGen("(eax <- esi = edi)" ->
          List("cmp %edi, %esi", "sete %al", "movzbl %al, %eax"))
  testInstructionGen("(ebx <- esi = edi)" ->
          List("cmp %edi, %esi", "sete %bl", "movzbl %bl, %ebx"))
  testInstructionGen("(ecx <- esi = edi)" ->
          List("cmp %edi, %esi", "sete %cl", "movzbl %cl, %ecx"))
  testInstructionGen("(edx <- esi = edi)" ->
          List("cmp %edi, %esi", "sete %dl", "movzbl %dl, %edx"))

  testInstructionGen("(eax <- 7 < 8)" -> List("movl $1, %eax"))
  testInstructionGen("(eax <- 7 <= 8)" -> List("movl $1, %eax"))
  testInstructionGen("(eax <- 7 = 8)" -> List("movl $0, %eax"))
  testInstructionGen("(eax <- 8 < 7)" -> List("movl $0, %eax"))
  testInstructionGen("(eax <- 8 <= 7)" -> List("movl $0, %eax"))
  testInstructionGen("(eax <- 8 <= 8)" -> List("movl $1, %eax"))
  testInstructionGen("(eax <- 8 = 7)" -> List("movl $0, %eax"))
  testInstructionGen("(eax <- 8 = 8)" -> List("movl $1, %eax"))

  testInstructionGen("(edx <- 7 < 8)" -> List("movl $1, %edx"))
  testInstructionGen("(edx <- 7 <= 8)" -> List("movl $1, %edx"))
  testInstructionGen("(edx <- 7 = 8)" -> List("movl $0, %edx"))
}

trait L1X86Test extends org.scalatest.FunSuite{

  val compiler = new L1Compiler with X86.X86Generator
  import compiler._

  def testInstructionGen(t: (String, List[String])): Unit = {
    test(t._1 + " => " + t._2){
      assert(genInst(compiler.parseInstruction(read(t._1))) === t._2)
    }
  }

  def testCompileFile(t: (String, String)) { testCompile(t._1, new File(t._1).read, t._2) }
  def testCompileString(t: (String, String)) { testCompile(t._1, t._1, t._2) }
  private def testCompile(testName: String, code: String, expectedResults: String): Unit = {
    test(testName + " => " + expectedResults){
      assert(X86.L1X86Runner.test(code) === expectedResults)
    }
  }

  def testCompilerVsInterpreter(filename: String) {
    val fullPath = Dir.L1File("1-test/" + filename)
    test(filename){ assert(X86.L1X86Runner.runFile(fullPath) === L1Interpreter.runFile(fullPath)) }
  }
}
