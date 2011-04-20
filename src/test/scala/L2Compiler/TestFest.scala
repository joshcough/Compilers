package L2Compiler

import io.FileHelper._
import io.Dir._
import java.io.File

class SpillTestFest extends TestFest(spillTestFestTests, spillTestFestResults, SpillMain.spill)
class LivenessTestFest extends TestFest(RobbyLivenessTests, RobbyLivenessResults, LivenessMain.liveness)

class L2TestFest2010 extends L2CompilerTest {

  object L1Interpreter {
    def run(file: File): String = {
      val (out, err) = io.CommandRunner("./src/test/compilers/interpreters/L1 " + file.getAbsolutePath)
      if (!(err startsWith "Welcome to L1")) error("interpreter died with the following errors:\n" + err)
      out
    }
  }

  new File("./tmp").mkdir

  for(((t, r), index) <- L2TestFest2010Tests.zip(L2TestFest2010Results).zipWithIndex){//}; if(index == 76)){
    test(index + "-" + t.getAbsolutePath){
      val code = t.read
      //println("L2 Code: " + code)
      val fileContainingCompilationResults = new File("./tmp/test" + index + ".L1")
      val originalL2File = new File("./tmp/test" + index + ".L2")
      val expectedResultFile = new File("./tmp/test" + index + ".res")
      val compilationResult = toCode(compile(code))
      //println("L1 Code: " + compilationResult)
      fileContainingCompilationResults.write(";" + t.getAbsolutePath + "\n" + compilationResult)
      originalL2File.write(code)
      expectedResultFile.write(r.read)
      verboseAssert(t.getAbsolutePath, L1Interpreter.run(fileContainingCompilationResults), r.read)
      // if we make it this far, delete the file.
      // the ones remaining are failures.
      fileContainingCompilationResults.delete()
      originalL2File.delete()
      expectedResultFile.delete()
      //val L1Code = toCode(compile(code))
      //println(actual)
      //val x86 = new L1Compiler.L1Compiler with L1Compiler.X86.X86Generator{}.compile(actual, "test")
      //println(x86)
    }
  }
}

abstract class TestFest(testFiles:Iterable[File],
                        resultFiles:Iterable[File],
                        f: String => String) extends L2CompilerTest {
  for((testFile, resultFile) <- testFiles.zip(resultFiles)) {
    test(testFile.getAbsolutePath){
      val code = testFile.read
      val myResult = f(code)
      val expectedResult = resultFile.read.replace("\n", "").replace("  ", " ")
      verboseAssert(code, myResult, expectedResult)
    }
  }
}