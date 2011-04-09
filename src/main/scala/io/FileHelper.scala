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
