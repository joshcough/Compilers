package L2Compiler

import L1Compiler.FileHelper._
import java.io.File

object HW2Creator {
  def main(args:Array[String]){
    var count = 0
    for(test <- new File("./spilltests.txt").read.split("\n")){
      new File("./test/test" + count + ".L2f").write(test + " x -4 s_")
      count += 1
    }
  }
}

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