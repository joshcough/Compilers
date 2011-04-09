package io

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