package L1Compiler

import java.io.{BufferedReader, InputStreamReader}

object CommandRunner {
  def apply(cmd: String): (String, String) = {
    val p = Runtime.getRuntime().exec(cmd)
    val stdOut = new BufferedReader(new InputStreamReader(p.getInputStream()))
    val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()))
    def readAll(br:BufferedReader): List[String] = {
      val next = br.readLine
      if(next == null) Nil else next :: readAll(br)
    }
    (readAll(stdOut).mkString("\n"), readAll(stdError).mkString("\n"))
  }

  def runAndDieOneErrors(cmd:String): String = {
    val (out, err) = CommandRunner(cmd)
    if(err != "") error("[" + cmd + "] died with the following errors:\n" + err) else out
  }
}

import java.io._
class FileHelper(file : File) {
  def write(text : String) : Unit = {
    val fw = new FileWriter(file)
    try{ fw.write(text) }
    finally{ fw.close }
  }
  def read: String = scala.io.Source.fromFile(file).getLines().mkString("\n")
}
object FileHelper{
  implicit def file2helper(file : File) = new FileHelper(file)
}