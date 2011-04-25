package L3Compiler

import util.TestHelpers

class L3ParserTests extends TestHelpers {
  
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

  testParse("((let ([x (+ 1 2)]) (let ([y (- 5 4)]) (* x y))))")
  
  testParse("((if 6 7 8))")

  testParse("((:fib 18) (:fib (x) x))")

  testParse("((let ([x 7]) x))")

  testParse("(8)")

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
}