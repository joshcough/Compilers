package L2Compiler

import L1Compiler.L1AST._
import L2Compiler.L2AST._
import L1Compiler.FileHelper._
import java.io.File
/**
//((x <- 1) (eax += x)) x -4 s
object HW2 {

  val compiler = new L2Compiler{
    def generateCode(ast:L2):L1 = error("TODO")
  }
  import compiler._

  def main(args:Array[String]){
    if(args(0) == "liveness") liveness(new File(args(1)).read)
    else if(args(0) == "spill") spilly(new File(args(1)).read)
    else error("wth")
  }

  //  % liveness f.L2f
  //  ((in (eax) (eax x)) (out (eax x) ()))
  def liveness(input:String){
    val res = inout(input)
    val inSet = res.map(_.in).map(s => "(" + s.map(_.toCode).mkString(" ") + ")").mkString(" ")
    val outSet = res.map(_.out).map(s => "(" + s.map(_.toCode).mkString(" ") + ")").mkString(" ")
    println("((in " + inSet + ") (out " + outSet + "))")
  }

  // % spill f.L2f
  //(((mem ebp -4) <- 1)
  // (s_0 <- (mem ebp -4))
  // (eax += s_0))
  def spilly(input:String){
    val (program, rest) = readWithRest(input)
    val List(varToSpill, offset, prefix) = read("(" + rest.toList.mkString.trim + ")").asInstanceOf[List[Any]]
    var newProgram = compiler.spill(
      Variable(varToSpill.toString.drop(1)), offset.toString.toInt,
      prefix.toString.drop(1), parseInstructionListThing(program.asInstanceOf[List[Any]]))
    println("(" + newProgram.map(_.toCode).mkString("\n") + ")")
  }
}
 **/