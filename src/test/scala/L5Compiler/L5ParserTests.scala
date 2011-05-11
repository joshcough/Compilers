package L5Compiler

import util.TestHelpers

import io.Dir._
import io.FileHelper._

class L5ParserTests extends TestHelpers with L5Compiler {

  for(f<-L5TestFest2010Tests) {
    test("parsing: " + f.getAbsolutePath) {
      //println(f.read)
      val code = read(f.read)
      //println(code)
      val actual = printSExp(read(toCode(parse(code))))
      val expected = printSExp(code)
      //println(actual)
      //println(expected)
      verboseAssert(f.getAbsolutePath, actual, expected)
    }
  }
}
