package L3Compiler

import util.{L2Interpreter, L3Interpreter, TestHelpers}

class L3CompilerTests extends TestHelpers {

//  testCompile("((print 7))", "(((eax <- (print 15)) (eax <- eax)))")
//
//  testCompile("((:f 10) (:f (x) (print x)))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(eax <- (print x))
//      |(eax <- eax)
//      |(return)))""")
//
//  testCompile("((let ([x 7]) (print x)))",
//    """
//      |(((x <- 15)
//      |(eax <- (print x))
//      |(eax <- eax)))""")
//
//  testCompile("((:f 10) (:f (x) (let ([y 7]) (print y))))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(y <- 15)
//      |(eax <- (print y))
//      |(eax <- eax)
//      |(return)))""")
//
//
//  testCompile("((+ 7 10))", "(((eax <- 15) (eax += 21) (eax -= 1)))")
//
//  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (+ x y)]) (print z)))))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(y <- 15)
//      |(z <- x)
//      |(z += y)
//      |(z -= 1)
//      |(eax <- (print z))
//      |(eax <- eax)
//      |(return)))""")
//
//  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (+ x y)]) z))))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(y <- 15)
//      |(z <- x)
//      |(z += y)
//      |(z -= 1)
//      |(eax <- z)
//      |(return)))""")
//
//  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (- x y)]) (print z)))))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(y <- 15)
//      |(z <- x)
//      |(z -= y)
//      |(z += 1)
//      |(eax <- (print z))
//      |(eax <- eax)
//      |(return)))""")
//
//  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (* x y)]) (print z)))))",
//    """
//      |(((ecx <- 21)
//      |(call :f)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(y <- 15)
//      |(__temp0 <- x)
//      |(__temp0 >>= 1)
//      |(z <- y)
//      |(z >>= 1)
//      |(z *= __temp0)
//      |(z <<= 1)
//      |(z += 1)
//      |(eax <- (print z))
//      |(eax <- eax)
//      |(return)))""")
//
//    testCompile("((let ([x :f]) (x 5)) (:f (x) (print x)))",
//    """
//      |(((x <- :f)
//      |(ecx <- 11)
//      |(call x)
//      |(eax <- eax))
//      |(:f
//      |(x <- ecx)
//      |(eax <- (print x))
//      |(eax <- eax)
//      |(return)))""")
//
//  testCompile("((let ([x (< 5 6)]) (print x)))",
//  """
//    |(((x <- 11)
//    |(x <- x < 13)
//    |(x <<= 1)
//    |(x += 1)
//    |(eax <- (print x))
//    |(eax <- eax)))""")
//
//  testCompile("((let ([x (new-array 5 5)]) (print x)))",
//  """
//    |(((eax <- (allocate 11 11))
//    |(x <- eax)
//    |(eax <- (print x))
//    |(eax <- eax)))""")

  testCompile("((let ([x (new-array 7 5)]) (let ([y (aset x 1 3)]) (print x))))",
  """
    |(((eax <- (allocate 15 11))
    |(x <- eax)
    |(y <- 3)
    |(y >>= 1)
    |(__temp0 <- (mem x 0))
    |(cjump __temp0 < y :__temp1 :__temp2)
    |:__temp1
    |(eax <- (array-error x y))
    |:__temp2
    |(y *= 4)
    |(y += x)
    |((mem y 4) <- 7)
    |(y <- 1)
    |(eax <- (print x))
    |(eax <- eax)))""")



  def testCompile(l3Code:String, expected:String) = {
    test(l3Code.clean){
      val compiler = new L3Compiler()
      import compiler._
      val l2Code = L2Compiler.L2Printer.toCode(compile(l3Code.clean))
      println("L2 Code: " + l2Code.clean)
      verboseAssert(l3Code, read(l2Code.clean).toString, read(expected.clean).toString)
      val l3InterpResult = L3Interpreter.run(l3Code.clean)
      val l2InterpResult = L2Interpreter.run(l2Code.clean)
      println("l3InterpResult: "+ l3InterpResult)
      println("l2InterpResult: "+ l2InterpResult)
      verboseAssert("l3 vs l2 interps", l3InterpResult, l2InterpResult)
    }
  }
}