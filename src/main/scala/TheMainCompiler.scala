import L2Compiler.{L2Printer, L2Compiler}
import L3Compiler.{L3Printer, L3Compiler}
import L4Compiler.{L4Printer, L4Compiler}
import L5Compiler.{L5Compiler}
import util.{Timer}

object TheMainCompiler extends Timer {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String) = compileToStrings(new File(filename).read)
  def compileToStrings(code:String): (String, String, String, String) = {
    val l5c = new L5Compiler{}
    val l4c = new L4Compiler{}
    val l3c = new L3Compiler{}
    val l2c = new L2Compiler{}

    timed("Complete compilation: ", {
      val l4 = alwaysTimed("L5 Compiler", l5c.compile(code))
      //    println("l4: " + L4Printer.toCode(l4))
      val l3 = alwaysTimed("L4 Compiler", l4c.compile(l4))
      //    println("l3: " + l3)
      val l2 = alwaysTimed("L3 Compiler", l3c.compile(l3))
      //    println("l2: " + L2Printer.toCode(l2))
      val l1String = alwaysTimed("L2 Compiler", L2Printer.toCode(l2c.compile(l2)))
      //    println("l1: " + l1String)
      (L4Printer.toCode(l4), L3Printer.toCode(l3), L2Printer.toCode(l2), l1String)
    })
  }
}
