package L2Compiler

import L2AST._

// ebx, esi, and edi are callee / function save
// eax, edx, and ecx are caller / application save / arguments (in that order)
/**
      val z1In = Assignment(Variable("z1"), ebx)
      val z2In = Assignment(Variable("z2"), edi)
      val z3In = Assignment(Variable("z3"), esi)
      val z1Out = Assignment(ebx, Variable("z1"))
      val z2Out = Assignment(edi, Variable("z2"))
      val z3Out = Assignment(esi, Variable("z3"))
*/
class L2CompilerTests extends L2CompilerTest {
//
//  import compiler._
//
//  test("liveness for: (((x <- 7)(eax <- (print x))))") {
//    val code = "(((x <- 7)(eax <- (print x))))"
//    assert(inout(code) === List(
//      InstuctionInOutSet(LabelDeclaration(Label("main")),Set(),Set()),
//      InstuctionInOutSet(Assignment(Variable("x"),Num(7)),Set(),Set(Variable("x"))),
//      InstuctionInOutSet(Assignment(eax, Print(Variable("x"))),Set(Variable("x")),Set())))
//  }
//
//  testCompile("(((x <- 7)(eax <- (print x))))" -> """
//(((eax <- ebx)
//(ebx <- edi)
//(edx <- esi)
//(ecx <- 7)
//(eax <- (print ecx))
//(ebx <- eax)
//(edi <- ebx)
//(esi <- edx))
//)""")
//
//  test("liveness for: (((x <- 7)(y <- 8)(eax <- (print x))))") {
//    val code = "(((x <- 7)(y <- 8)(eax <- (print x))))"
//    assert(inout(code) === List(
//      InstuctionInOutSet(LabelDeclaration(Label("main")),Set(),Set()),
//      InstuctionInOutSet(Assignment(Variable("x"),Num(7)),Set(),Set(Variable("x"))),
//      InstuctionInOutSet(Assignment(Variable("y"),Num(8)),Set(Variable("x")),Set(Variable("x"))),
//      InstuctionInOutSet(Assignment(eax, Print(Variable("x"))),Set(Variable("x")),Set())))
//  }
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
abstract class L2CompilerTest extends org.scalatest.FunSuite{

  val compiler = new L2Compiler{
    def generateCode(ast:L2):String = error("TODO")
  }
  import compiler._

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
//
//  def testCompile(t:(String, String)): Unit = {
//    test(t._1){
//      val out = L2Printer.toCode(compile(t._1)).trim
//      println(out)
//      assert(out === t._2.trim)
//    }
//  }
}