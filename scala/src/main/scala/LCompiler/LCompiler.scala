package LCompiler

import L2Compiler.{L2Printer, L2Compiler}
import L3Compiler.{L3Printer, L3Compiler}
import L4Compiler.{L4Printer, L4Compiler}
import L5Compiler.{L5Compiler}
import util.{Timer}
import L1Compiler.L1Compiler
import L1Compiler.X86.{L1X86Runner, X86Generator}

trait LNCompiler extends io.Reader {
  def compileToString(code:String):String
}

object LCompiler extends LNCompiler with Timer {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) {
    L1X86Runner.finishCompilation(compileToStrings(new File(args(0)).read)(0))
  }

  def compileToString(code:String): String = compileToStrings(code).last

  def compileToStrings(code:String, compileTo:Int=0): List[String] = {

    val allCompilers =
      List(new L1Compiler with X86Generator{}, new L2Compiler{}, new L3Compiler{}, new L4Compiler{}, new L5Compiler{})

    val allCompilersWithIndex = allCompilers.zipWithIndex.map{ case (c,i) => (c, i + 1) }.reverse

    val numCompilersToUse = 5 - compileTo
    val compilers = allCompilersWithIndex.take(numCompilersToUse)

    timed("Complete compilation: ", {
      compilers.foldLeft(List(code)){ case (nextCode :: codes, (comp, level)) =>
        timed("L" + level + " Compiler", comp.compileToString(nextCode) :: codes)
      }
    })
  }
}

//  def compileToString(code:String): (String, String, String, String) = {
//    val l5c = new L5Compiler{}
//    val l4c = new L4Compiler{}
//    val l3c = new L3Compiler{}
//    val l2c = new L2Compiler{}
//
//    timed("Complete compilation: ", {
//      val l4 = alwaysTimed("L5 Compiler", l5c.compile(code))
//      //    println("l4: " + L4Printer.toCode(l4))
//      val l3 = alwaysTimed("L4 Compiler", l4c.compile(l4))
//      //    println("l3: " + l3)
//      val l2 = alwaysTimed("L3 Compiler", l3c.compile(l3))
//      //    println("l2: " + L2Printer.toCode(l2))
//      val l1String = alwaysTimed("L2 Compiler", L2Printer.toCode(l2c.compile(l2)))
//      //    println("l1: " + l1String)
//      (L4Printer.toCode(l4), L3Printer.toCode(l3), L2Printer.toCode(l2), l1String)
//    })
//  }
