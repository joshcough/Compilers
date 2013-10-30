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

object L1Interpreter extends Interpreter(1)
object L2Interpreter extends Interpreter(2)
object L3Interpreter extends Interpreter(3)
object L4Interpreter extends Interpreter(4)
object L5Interpreter extends Interpreter(5)

object Interpreter{
  val interps = Map(1->L1Interpreter, 2->L2Interpreter, 3->L3Interpreter, 4->L4Interpreter, 5->L5Interpreter)
}

case class Interpreter(level:Int) {
  val name = "L" + level
  def getCompiledCodeInterpreter = Interpreter.interps(level-1)
  def run(file: File): String = {
    val command = "./src/test/compilers/interpreters/" + name+ " " + file.getAbsolutePath
    //println("command: " + command)
    val (out, err) = io.CommandRunner(command)
    if (!(err startsWith "Welcome to " + name)) error(name + " interpreter died with the following errors:\n" + err)
    out + (err.split("\n").filter(_.startsWith("attempted")).mkString("\n"))
  }
  def run(code:String): String = {
    val f = new File("/tmp/test." + name)
    f.write(code)
    run(f)
  }
}