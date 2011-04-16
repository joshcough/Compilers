package L2Compiler

import L2AST._

class L2CompilerTests extends L2CompilerTest {

  testCompile(
    input =
    """(((x <- 7)
        |(eax <- (print x))
        |(return)))""",
    expected=
    """(((eax <- 7)
        |(eax <- (print eax))
        |(return)))""")

//
//  // interesting case here.... y isnt in the in or out set anywhere...
//  // why? shouldnt it be in the out set of its own assignment statement? maybe not...
//  // this could possibly be a case where we can whack that entire statement altogether,
//  // because it didnt appear in the in or out set.
//  testCompile("(((x <- 7)(y <- 8)(eax <- (print x))))" -> """
//(((edx <- ebx)
//(ecx <- edi)
//(eax <- esi)
//(ebx <- 7)
//(print ebx)
//(ebx <- edx)
//(edi <- ecx)
//(esi <- eax))
//)""")

}


//((x <- 1) (eax += x)) x -4 s
abstract class L2CompilerTest extends org.scalatest.FunSuite with L2CompilerExtras {

  def End = None // sort of hacky, but whatever.
  def Just(i:Int) = Some(i)
  implicit def pimpedString(s:String) = new {
    def clean = s.stripMargin.trim
  }

  def testParseSExpr(t: (Any, L2)){
    test(t._1 + " => " + t._2){ assert(parse(t._1) === t._2) }
  }

  def testParse(t: (String, L2)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseProgram(t._1) === t._2) }
  }

  def testParseS(t: (Any, S)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseS(t._1) === t._2) }
  }

  def testParseInstruction(t: (String, Instruction)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseInstruction(read(t._1)) === t._2) }
  }

  def testParseInstructionError(t: (String, String)): Unit  = {
    test(t._1 + " => " + t._2){
      val ex = intercept[Exception] { parseInstruction(read(t._1)) }
      assert(ex.getMessage === t._2)
    }
  }

  def testCompile(input:String, expected:String): Unit = {
    test(input){
      val actual = L2Printer.toCode(compile(input.clean))
      if(actual != expected.clean){
        println("compile test failed")
        println("code: " + input.clean)
        println("expected: " + expected.clean)
        println("actual: " + actual)
      }
      assert(actual === expected.clean)
    }
  }
}