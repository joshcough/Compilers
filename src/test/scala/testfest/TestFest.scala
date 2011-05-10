package testfest

import io.FileHelper._
import io.Dir._
import java.io.File
import L3Compiler.L3Compiler
import io.Reader
import L2Compiler.{InterferenceMain, L2CompilerTest, L2Compiler, LivenessMain, SpillMain}
import L4Compiler.L4Compiler
import util.{L4Interpreter, TestHelpers, L3Interpreter, L2Interpreter, Interpreter, L1Interpreter}

class SpillTestFest extends L2TestFest(spillTestFestTests, spillTestFestResults, SpillMain.spill)
class LivenessTestFest extends L2TestFest(livenessTestFestTests, livenessTestFestResults, LivenessMain.liveness)

class GraphTestFest extends L2CompilerTest {
  for((testFile, resultFile) <- graphTestFestTests.zip(graphTestFestResults)) {
    test(testFile.getAbsolutePath){
      val code = testFile.read
      val (i, a) = InterferenceMain.interferenceAndAllocation(code)
      val (actualInter, actualAlloc) = (read(i), printSExp(read(a)))
      val resultFileContents = resultFile.read
      val (expectedInter, rest) = readWithRest(resultFileContents)
      val expectedAlloc = printSExp(read(rest))
      verboseAssert("interferences for code:\n" + code, printSExp(actualInter), printSExp(expectedInter))
//      def isFail(s:String) = s contains "#f"
//      if(isFail(actualAlloc) || isFail(expectedAlloc) ){
//        verboseAssert("allocations for code:\n" + code, actualAlloc, expectedAlloc)
//      }
    }
  }
}

//TODO: add L1 here

class L2TestFest2010 extends TestFest2010(
        new L2Compiler{}.compileToString, L2Interpreter,
        L2TestFest2010Tests, L2TestFest2010Results)

class L3TestFest2010 extends TestFest2010(
  new L3Compiler().compileToString, L3Interpreter,
  L3TestFest2010Tests, L3TestFest2010Results)

class L4TestFest2010 extends TestFest2010(
  new L4Compiler{}.compileToString, L4Interpreter,
  L4TestFest2010Tests, L4TestFest2010Results)

abstract class TestFest2010(compile: String => String,
                   language:Interpreter,
                   testFiles:Iterable[File],
                   resultFiles:Iterable[File]) extends TestHelpers with util.SlowTest{

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

abstract class L2TestFest(testFiles:Iterable[File],
                        resultFiles:Iterable[File],
                        f: String => String) extends TestFest(testFiles, resultFiles, f) with L2CompilerTest

abstract class TestFest(testFiles:Iterable[File],
                        resultFiles:Iterable[File],
                        f: String => String) extends Reader with TestHelpers {
  for((testFile, resultFile) <- testFiles.zip(resultFiles)) {
    test(testFile.getAbsolutePath){
      val code = testFile.read
      val myResult = f(code)
      val expectedResult = resultFile.read.replace("\n", "").replace("  ", " ")
      verboseAssert(code, myResult, expectedResult)
    }
  }
}
