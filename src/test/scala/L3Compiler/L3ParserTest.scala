package L3Compiler

import org.scalatest.FunSuite

class L3ParserTests extends FunSuite{
  testParse("""
  ((:fib 18)
   (:fib (x)
     (let ([xlt2 (< x 2)])
       (if xlt2
         1
         (let ([x1 (- x 1)])
           (let ([f1 (:fib x1)])
             (let ([x2 (- x 2)])
               (let ([f2 (:fib x2)])
                 (+ f1 f2)))))))))""")
  testParse("((if 6 7 8))")

  testParse("((:fib 18) (:fib (x) x))")

  testParse("((let ([x 7]) x))")

  def testParse(program:String): Unit = {
    val parser = new L3Parser with L3Printer with io.Reader{}
    import parser._
    test(program){
      //println("input code = " + program)
      val expected = read(program)
      val resultCode = toCode(parse(read(program)))
      //println("resultCode: " + resultCode)
      verboseAssert(program, read(resultCode).toString, expected.toString)
    }
  }

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