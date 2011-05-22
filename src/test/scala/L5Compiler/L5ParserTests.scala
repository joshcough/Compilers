package L5Compiler

import util.TestHelpers

import io.Dir._
import io.FileHelper._

class L5ParserTests extends TestHelpers with L5Compiler {

  for(f<-L5TestFest2010Tests) testParse(code=f.read, testName=Some(f.getAbsolutePath))

  def testParse(code:String, expected:Option[String] = None, testName: Option[String]=None) = {
    def roundTrip(code:String) = printSExp(read(toCode(parse(read(code)))))
    def oneWay(code:String) = printSExp(read(code))
    test("parsing: " + testName.getOrElse(code)) {
      verboseAssert(code, roundTrip(code), expected.getOrElse(oneWay(code)))
    }
  }

  testParse("(lambda (x) x)", expected=Some("(lambda (x) x)"))
}
