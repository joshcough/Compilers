package L2Compiler
//
//import L2Compiler.L2AST._
//import io.FileHelper._
//
//import java.io.File
////((x <- 1) (eax += x)) x -4 s
//object HW2 {
//
//  val compiler = new L2Compiler{}
//  import compiler._
//
//  def main(args:Array[String]){
//    if(args(0) == "liveness") liveness(new File(args(1)).read)
//    else error("wth")
//  }
//
//  //  % liveness f.L2f
//  //  ((in (eax) (eax x)) (out (eax x) ()))
//  def liveness(input:String){
//    val res = inout(input)
//    val inSet = res.map(_.in).map(s => "(" + s.map(_.toCode).mkString(" ") + ")").mkString(" ")
//    val outSet = res.map(_.out).map(s => "(" + s.map(_.toCode).mkString(" ") + ")").mkString(" ")
//    println("((in " + inSet + ") (out " + outSet + "))")
//  }
//}
