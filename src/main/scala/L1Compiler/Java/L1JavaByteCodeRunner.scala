package L1Compiler.Java

import L1Compiler.{CommandRunner, FileHelper, L1Compiler, Runner}
import CommandRunner._
import FileHelper._
import java.io.File

object L1JavaByteCodeRunner extends Runner {

  def main(args:Array[String]){ runFile(args(0)) }

  def run(code:String, originalFileName:String): String = {
    val compiler = new L1Compiler with JavaByteCodeGenerator
    val unitName = originalFileName.reverse.takeWhile(_!='/').dropWhile(_!='.').drop(1).reverse
    val jFileName = unitName + ".j"
    new File(jFileName).write(compiler.compile(code, originalFileName))
    jasmin.Main.main(Array(jFileName))
    val classpath = ".:./out/production/compilers/:./project/boot/scala-2.8.1/lib/scala-library.jar"
    runAndDieOneErrors("java -cp " + classpath + " " + unitName)
  }
}
// java -cp .:./target/scala_2.8.1/classes:./project/boot/scala-2.8.1/lib/scala-library.jar Test