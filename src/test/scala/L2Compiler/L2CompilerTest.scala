package L2Compiler

import L2AST._

abstract class L2CompilerTest extends org.scalatest.FunSuite{
  val compiler = new L2Compiler{
    def generateCode(ast:L2):String = error("TODO")
  }
  import compiler._

  def parseProgram(s:String) = parse(read(s))
  def parseInstructionListThing(s:String) = parseInstructionList(read(s).asInstanceOf[List[Any]])

  def testParseSExpr(t: (Any, L2)){
    test(t._1 + " => " + t._2){ assert(parse(t._1) === t._2) }
  }

  def testParse(t: (String, L2)): Unit = {
    test(t._1 + " => " + t._2){ assert(parseProgram(t._1) === t._2) }
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

  def inout(code:String) = compiler.inoutHack(parseInstructionListThing(code))
  def interferingVariables(code:String) = buildInterferenceSet(inout(code)).filter{
    case (x:Variable,y:Variable) => true
    case _ => false
  }

  def attemptToColor(code:String) = {
    RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(code))).color
  }

  def spill(code:String) = compiler.spill(Variable("x"), -4, "s_", parseInstructionListThing(code))
  def testSpill(code:String, expected: Instruction*) = test(code){
    val newProgramList = spill(code)
    //println(newProgram)
    assert(newProgramList === expected.toList)
  }


  //((x <- 1) (eax += x)) x -4 s
  
}