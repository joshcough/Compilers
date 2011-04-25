package util

import org.scalatest.FunSuite
import java.io.File
import io.FileHelper._

trait TestHelpers extends FunSuite {
  implicit def pimpedString(s:String) = new {
    def clean = s.stripMargin.trim
  }

  def verboseAssert(code:String, actual: String, expected: String) {
    if (actual.clean != expected.clean) {
      println("code:\n" + code.clean)
      println("actual:\n" + actual.clean)
      println("expected:\n" + expected.clean)
    }
    assert(actual.clean === expected.clean)
  }
}

object L1Interpreter extends Interpreter("L1")
object L2Interpreter extends Interpreter("L2")
object L3Interpreter extends Interpreter("L3")

class Interpreter(name:String) {
  def run(file: File): String = {
    val (out, err) = io.CommandRunner("./src/test/compilers/interpreters/" + name+ " " + file.getAbsolutePath)
    if (!(err startsWith "Welcome to " + name)) error(name + " interpreter died with the following errors:\n" + err)
    out
  }
  def run(code:String): String = {
    val f = new File("/tmp/test." + name)
    f.write(code)
    run(f)
  }
}