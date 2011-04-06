package L2Compiler

import L1Compiler.FileHelper._
import java.io.File

import L1Compiler.CommandRunner._

object HW2TestResultsCreator{
  def main(args:Array[String]){
    for(f <- new File("./test").listFiles; if(f.getName.endsWith(".L2f"))){
      val livenessResFile = "./test/" + f.getName.dropRight(3) + "lres"
      val spillResFile = "./test/" + f.getName.dropRight(3) + "sres"
      new File(spillResFile).write(runAndDieOneErrors("./spill " + "./test/" + f.getName))
      new File(livenessResFile).write(runAndDieOneErrors("./liveness " + "./test/" + f.getName))
    }
  }
}