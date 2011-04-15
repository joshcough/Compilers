package io

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

object Dir {
  val L1 = "./src/test/compilers/L1/"
  val L2 = "./src/test/compilers/L2/"
  def L1TestFiles = filesInDir(L1+ "/1-test", "L1")
  def RobbyLivenessTests = filesInDir(L2+ "/robby-liveness-test", "L2f")
  def RobbyLivenessResults = filesInDir(L2+ "/robby-liveness-test", "lres")
  def filesInDir(dir:String, ending:String) = {
    val parent = new File(dir)
    parent.list.toList.filter(_.endsWith(ending)).sorted.map(name => new File(parent, name))
  }
  def L1File(name:String) = Dir.L1 + name
  def L2File(name:String) = Dir.L2 + name
}
