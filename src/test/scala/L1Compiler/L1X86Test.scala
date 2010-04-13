package L1Compiler

import reader._
import L1AST._
import FileHelper._
import java.io.File

class GeneratorTests extends GenMathInstructionsTest with
        RegisterAssigmentInstructionsTest with GenFullProgramTest

class TestCompilerVsInterpreter extends L1X86Test{
  val testFiles =  List(
    "add-two-registers.L1",
    "compare-negs.L1",
    "labels-fun.L1",
    "simple-cjump-false.L1",
    "simple-mem-write.L1",
    "allocate-size-1000-array.L1",
    "double-decrement.L1",
    "neg.L1",
    "simple-cjump-truth.L1",
    "simple-mult-2.L1",
    "allocate-size-1-array.L1",
    "fun-with-arrays.L1",
    "print-eax.L1",
    "simple-decrement.L1",
    "simple-mult.L1",
    "allocate-size-2-array.L1",
    "fun-with-arrays-part-2.L1",
    "print-one.L1",
    "simple-increment-eax.L1",
    "simple-stack-stuff.L1",
    "cjump-on-registers.L1",
    "gotos.L1",
    "print-zero.L1",
    "simple-mem-read.L1",
    "use-stack-pointer.L1")
  testFiles.foreach(testCompilerVsInterpreter)
}

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
  def stripComments(code:String) = code.split("\n").map(s => s.takeWhile(_!=';').trim).mkString
  def read(code:String): Any = {
    //println("stripping: " + code)
    val stripped = stripComments(code)
    //println("reading: " + stripped)
    val r = new Reader().read(stripped)
    //println("read: " + r)
    r
  }
  def parseInstruction(a:Any) = parser parseInstruction a
  def parse(a:Any) = {
    //println("parsing: " + a)
    parser parse a
  }
  def generateCodeForInstruction(i:Instruction) = L1X86Generator.generateCode(i)
  def generateCode(program:L1) = L1X86Generator.generateCode(program)

  def testInstructionGen(t: (String, String)): Unit = {
    test(t._1 + " => " + t._2){
      assert(generateCodeForInstruction(parseInstruction(read(t._1))) === t._2)
    }
  }

  type Results = String

  def testCompileFile(t: (String, String)) { testCompile(t._1, new File(t._1).read, t._2) }
  def testCompileString(t: (String, String)) { testCompile(t._1, t._1, t._2) }
  private def testCompile(testName: String, code: String, expectedResults: String): Unit = {
    test(testName + " => " + expectedResults){
      assert(compileAndRunCode(code) === expectedResults)
    }
  }

  import CommandRunner._

  private def compileAndRunCode(code:String): Results = {
    new File("/tmp/test.S").write(generateCode(parse(read(code))))
    runAndDieOneErrors("gcc -O2 -c -o /tmp/runtime.o ./src/main/compilers/L1/runtime.c")
    runAndDieOneErrors("as -o /tmp/test.o /tmp/test.S")
    runAndDieOneErrors("gcc -o /tmp/a.out /tmp/test.o /tmp/runtime.o")
    runAndDieOneErrors("/tmp/a.out")
  }

  def testCompilerVsInterpreter(filename: String) {
    def L1File(name:String) = "./src/main/compilers/L1/" + name
    def runInterpreter = {
      val (out, err) = CommandRunner(L1File("L1") + " " + "code/" + filename)
      if(err != "Welcome to L1, v5") error("interpreter died with the following errors:\n" + err)
      out
    }
    def runCompilerGeneratedCode = compileAndRunCode(new File(L1File("code/" + filename)).read)
    test(filename){ assert(runCompilerGeneratedCode === runInterpreter) }
  }
}
