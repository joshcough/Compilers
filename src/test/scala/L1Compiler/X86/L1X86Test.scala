package L1Compiler.X86

import L1Compiler._

class TestCompilerVsInterpreter extends L1X86Test{
  import L1Compiler.FileHelper._
  //Dir.testFiles.filter(_.contains("labels-on-heap.L1")).foreach(testCompilerVsInterpreter)
  //Dir.testFiles.foreach(testCompilerVsInterpreter)
}

class RunFullProgramTest extends L1X86Test {
//  testCompileAndRunString("(((eax <- 5)(eax <- (print eax))))" -> "2")
//  testCompileAndRunString("(((eax <- (allocate 3 3))(eax <- (print eax))))" -> "{s:1, 1}")
}

class CompileFullProgramTest extends L1X86Test {
  testCompileString(""";;10
(((eax <- 19) (eax <- (print eax))))""" -> """movl $19, %eax
pushl %eax
call print
addl $4, %esp""")

  testCompileString(""";;10
(((eax <- 18) (eax += 1) (eax <- (print eax))))""" -> """movl $18, %eax
addl $1, %eax
pushl %eax
call print
addl $4, %esp""")

  testCompileString("""(((esi <- 3) (eax <- (allocate esi 3)) (eax <- (array-error eax 3))))""" ->
    """movl $3, %esi
pushl $3
pushl %esi
call allocate
addl $8, %esp
pushl $3
pushl %eax
call print_error
addl $8, %esp""")

  testCompileString("(((eax <- -2147483648) (eax -= 2) (eax <- (print eax))))" ->
    """movl $-2147483648, %eax
subl $2, %eax
pushl %eax
call print
addl $4, %esp""")
  
  testCompileString("(((eax <- -2147483648) (eax -= 3) (eax <- (print eax))))" ->
    """movl $-2147483648, %eax
subl $3, %eax
pushl %eax
call print
addl $4, %esp""")
  
  testCompileString("(((eax <- (allocate 5 1)) (eax <- (array-error eax 17))))" ->
    """pushl $1
pushl $5
call allocate
addl $8, %esp
pushl $17
pushl %eax
call print_error
addl $8, %esp""")

  testCompileString("(((eax <- (allocate 7 1)) (eax <- (array-error eax 19)) (eax <- (print 3))))" ->
    """pushl $1
pushl $7
call allocate
addl $8, %esp
pushl $19
pushl %eax
call print_error
addl $8, %esp
pushl $3
call print
addl $4, %esp""")

  testCompileString("(((eax <- (allocate 3 1)) (eax <- (array-error eax 5)) (eax <- (print eax))))" ->
    """pushl $1
pushl $3
call allocate
addl $8, %esp
pushl $5
pushl %eax
call print_error
addl $8, %esp
pushl %eax
call print
addl $4, %esp""")

  testCompileString(""";; #18 multiple allocations, array-errors
(((ebx <- 25)
  (eax <- (allocate ebx 1))
  (esi <- eax)
  (edi <- 25)
  (eax <- (allocate edi 1))
  (cjump edi < 28 :_1 :_2)
  :_1
  (eax <- (array-error eax 1))
  (goto :_3)
  :_2
  (eax <- (array-error 1 eax))
  (goto :_3)
  :_3))""" -> """movl $25, %ebx
pushl $1
pushl %ebx
call allocate
addl $8, %esp
movl %eax, %esi
movl $25, %edi
pushl $1
pushl %edi
call allocate
addl $8, %esp
cmpl $28, %edi
jl L1__1
jmp L1__2
L1__1:
pushl $1
pushl %eax
call print_error
addl $8, %esp
jmp L1__3
L1__2:
pushl %eax
pushl $1
call print_error
addl $8, %esp
jmp L1__3
L1__3:""")
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

  //import java.io.File
  //def testCompileFile(t: (String, String)) { testCompile(t._1, new File(t._1).read, t._2) }
  def testCompileString(t: (String, String)) { testCompile(t._1, t._1, t._2) }
  private def testCompile(testName: String, code: String, expectedResults: String): Unit = {
    test(testName + " => " + expectedResults){
      val ast = compiler.parse(compiler.read(code))
      assert(ast.main.body.flatMap(compiler.genInst).mkString("\n") === expectedResults)
    }
  }


  def testCompileAndRunString(t: (String, String)) { testCompileAndRun(t._1, t._1, t._2) }
  private def testCompileAndRun(testName: String, code: String, expectedResults: String): Unit = {
    test(testName + " => " + expectedResults){
      assert(X86.L1X86Runner.test(code) === expectedResults)
    }
  }

  def testCompilerVsInterpreter(filename: String) {
    val fullPath = Dir.L1File("1-test/" + filename)
    test(filename){ assert(X86.L1X86Runner.runFile(fullPath) === L1Interpreter.runFile(fullPath)) }
  }
}
