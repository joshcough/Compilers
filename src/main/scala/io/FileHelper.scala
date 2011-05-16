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
  val testDir = "./src/test/compilers/"
  val L1 = testDir + "L1/"
  val L2 = testDir + "L2/"
  val testFest = testDir + "test-fest/"

  def L1File(name:String) = Dir.L1 + name
  def L2File(name:String) = Dir.L2 + name
  def L1TestFiles = filesInDir(L1 + "/1-test", "L1")

  def spillTestFestTests = filesInDir(testFest + "spill-test", "L2f")
  def spillTestFestResults = filesInDir(testFest + "spill-test", "sres")

  def livenessTestFestTests = filesInDir(testFest + "liveness-test", "L2f")
  def livenessTestFestResults = filesInDir(testFest + "liveness-test", "lres")

  def graphTestFestTests = filesInDir(testFest + "graph-test", "L2f")
  def graphTestFestResults = filesInDir(testFest + "graph-test", "gres")

  def L2TestFest2010Tests = filesInDir(testFest + "L2-tests-from-2010", "L2")
  def L2TestFest2010Results = filesInDir(testFest + "L2-tests-from-2010", "res")

  def L2TestFest2011Tests = filesInDir(testFest + "2-test", "L2")
  def L2TestFest2011Results = filesInDir(testFest + "2-test", "L1")

  def L3TestFest2010Tests = filesInDir(testFest + "L3-tests-from-2010", "L3")
  def L3TestFest2010Results = filesInDir(testFest + "L3-tests-from-2010", "res")

  def L3TestFest2011Tests = filesInDir(testFest + "3-test", "L3")
  def L3TestFest2011Results = filesInDir(testFest + "3-test", "L2")

  def L4TestFest2010Tests = filesInDir(testFest + "L4-tests-from-2010", "L4")
  def L4TestFest2010Results = filesInDir(testFest + "L4-tests-from-2010", "res")

  def L5TestFest2010Tests = filesInDir(testFest + "L5-tests-from-2010", "L5")
  def L5TestFest2010Results = filesInDir(testFest + "L5-tests-from-2010", "res")

  def filesInDir(dir:String, ending:String): Iterable[File] = filesInDir(new File(dir), ending)
  def filesInDir(parent:File, endingWith:String): Iterable[File] = {
    filesInDir(parent).filter(_.getName.endsWith(endingWith))
  }
  def filesInDir(parent:File): Iterable[File] = {
    parent.listFiles.flatMap{f => if(f.isDirectory) filesInDir(f) else List(f)}
            .sortWith(_.getAbsolutePath < _.getAbsolutePath)
  }
}
