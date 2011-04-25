package L2Compiler

import io.FileHelper._
import io.Dir._
import java.io.File
import util.{TestHelpers, L3Interpreter, L2Interpreter, Interpreter, L1Interpreter}
import L3Compiler.L3Compiler

class SpillTestFest extends TestFest(spillTestFestTests, spillTestFestResults, SpillMain.spill)
class LivenessTestFest extends TestFest(livenessTestFestTests, livenessTestFestResults, LivenessMain.liveness)

//TODO: add L1 here

class L2TestFest2010 extends TestFest2010(
        new L2Compiler{}.compileToString, L2Interpreter,
        L2TestFest2010Tests, L2TestFest2010Results)

class L3TestFest2010 extends TestFest2010(
  new L3Compiler().compileToString, L3Interpreter,
  L3TestFest2010Tests, L3TestFest2010Results)

class TestFest2010(compile: String => String,
                   language:Interpreter,
                   testFiles:Iterable[File],
                   resultFiles:Iterable[File]) extends TestHelpers {

  io.CommandRunner("rm -rf ./tmp/"+language.name+"/")
  new File("./tmp/"+language.name+"/").mkdir

  for(((t, r), index) <- testFiles.zip(resultFiles).zipWithIndex){
    test(index + "-" + t.getAbsolutePath){
      val code = t.read
      val fileContainingCompilationResults =
        new File("./tmp/"+language.name+"/" + index + "." + language.getCompiledCodeInterpreter.name)
      val compilationResult = compile(code)
      val commentedOutCode = code.split("\n").map("; " + _).mkString("\n") + "\n\n"
      val commentedOutResult = r.read.split("\n").map("; " + _).mkString("\n") + "\n\n"
      fileContainingCompilationResults.write(commentedOutCode + commentedOutResult + compilationResult)
      val interpreterResults = language.getCompiledCodeInterpreter.run(fileContainingCompilationResults)
      //println(interpreterResults)
      verboseAssert(t.getAbsolutePath, interpreterResults, r.read)
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