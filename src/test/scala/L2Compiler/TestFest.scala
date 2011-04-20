package L2Compiler

import io.FileHelper._
import io.Dir._
import java.io.File

class SpillTestFest extends TestFest(spillTestFestTests, spillTestFestResults, SpillMain.spill)
class LivenessTestFest extends TestFest(RobbyLivenessTests, RobbyLivenessResults, LivenessMain.liveness)

class L2TestFest2010 extends L2CompilerTest {
  for(t<-L2TestFest2010Tests){
    test(t.getAbsolutePath){
      val actual = toCode(compile(t.read))
      println(actual)
      val x86 = new L1Compiler.L1Compiler with L1Compiler.X86.X86Generator{}.compile(actual, "test")
      println(x86)
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