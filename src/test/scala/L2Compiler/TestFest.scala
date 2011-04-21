package L2Compiler

import io.FileHelper._
import io.Dir._
import java.io.File

class SpillTestFest extends TestFest(spillTestFestTests, spillTestFestResults, SpillMain.spill)
class LivenessTestFest extends TestFest(livenessTestFestTests, livenessTestFestResults, LivenessMain.liveness)

class L2TestFest2010 extends L2CompilerTest {

  object L1Interpreter {
    def run(file: File): String = {
      val (out, err) = io.CommandRunner("./src/test/compilers/interpreters/L1 " + file.getAbsolutePath)
      if (!(err startsWith "Welcome to L1")) error("interpreter died with the following errors:\n" + err)
      out
    }
  }

  new File("./tmp").mkdir

  for(((t, r), index) <- L2TestFest2010Tests.zip(L2TestFest2010Results).zipWithIndex ; if(index < 730)){
    test(index + "-" + t.getAbsolutePath){
      val code = t.read
      val fileContainingCompilationResults = new File("./tmp/" + index + ".L1")
      val compilationResult = toCode(compile(code))
      val commentedOutCode = code.split("\n").map("; " + _).mkString("\n") + "\n\n"
      val commentedOutResult = r.read.split("\n").map("; " + _).mkString("\n") + "\n\n"
      fileContainingCompilationResults.write(commentedOutCode + commentedOutResult + compilationResult)
      verboseAssert(t.getAbsolutePath, L1Interpreter.run(fileContainingCompilationResults), r.read)
      // if we make it this far, delete the file.
      // the ones remaining are failures.
      fileContainingCompilationResults.delete()
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