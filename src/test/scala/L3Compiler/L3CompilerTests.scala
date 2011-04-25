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
//
//  testCompile("((let ([x (new-array 7 5)]) (let ([y (aset x 1 3)]) (print x))))",
//  """
//    |(((eax <- (allocate 15 11))
//    |(x <- eax)
//    |(y <- 3)
//    |(y >>= 1)
//    |(__temp0 <- (mem x 0))
//    |(cjump __temp0 < y :__temp1 :__temp2)
//    |:__temp1
//    |(eax <- (array-error x y))
//    |:__temp2
//    |(y *= 4)
//    |(y += x)
//    |((mem y 4) <- 7)
//    |(y <- 1)
//    |(eax <- (print x))
//    |(eax <- eax)))""")
//
//  testCompile("((let ([x (new-array 7 5)]) (let ([y (alen x)]) (print y))))",
//  """
//    |(((eax <- (allocate 15 11))
//    |(x <- eax)
//    |(y <- (mem x 0))
//    |(y <<= 1)
//    |(y += 1)
//    |(eax <- (print y))
//    |(eax <- eax)))""")
//
//  testCompile("((let ([x (new-array 7 5)]) (let ([y (aref x 2)]) (print y))))",
//  """
//    |(((eax <- (allocate 15 11))
//    |(x <- eax)
//    |(y <- (mem x 8))
//    |(eax <- (print y))
//    |(eax <- eax)))""")

 testCompile("((let ([x (new-tuple 3)]) (print x)))",
  """
    |(((eax <- (allocate 3 1))
(x <- eax)
(__temp0 <- 1)
(__temp0 >>= 1)
(__temp1 <- (mem x 0))
(cjump __temp1 < __temp0 :__temp2 :__temp3)
:__temp2
(eax <- (array-error x __temp0))
:__temp3
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- 7)
(__temp0 <- 1)
(eax <- (print x))
(eax <- eax)))""")


  testCompile("((let ([x (new-tuple 3 29 0 -5 -1 1)]) (print x)))",
  """
    |(((eax <- (allocate 13 1))
(x <- eax)
(__temp0 <- 1)
(__temp0 >>= 1)
(__temp1 <- (mem x 0))
(cjump __temp1 < __temp0 :__temp2 :__temp3)
:__temp2
(eax <- (array-error x __temp0))
:__temp3
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- 7)
(__temp0 <- 1)
(__temp0 <- 3)
(__temp0 >>= 1)
(__temp4 <- (mem x 0))
(cjump __temp4 < __temp0 :__temp5 :__temp6)
:__temp5
(eax <- (array-error x __temp0))
:__temp6
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- 59)
(__temp0 <- 1)
(__temp0 <- 5)
(__temp0 >>= 1)
(__temp7 <- (mem x 0))
(cjump __temp7 < __temp0 :__temp8 :__temp9)
:__temp8
(eax <- (array-error x __temp0))
:__temp9
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- 1)
(__temp0 <- 1)
(__temp0 <- 7)
(__temp0 >>= 1)
(__temp10 <- (mem x 0))
(cjump __temp10 < __temp0 :__temp11 :__temp12)
:__temp11
(eax <- (array-error x __temp0))
:__temp12
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- -9)
(__temp0 <- 1)
(__temp0 <- 9)
(__temp0 >>= 1)
(__temp13 <- (mem x 0))
(cjump __temp13 < __temp0 :__temp14 :__temp15)
:__temp14
(eax <- (array-error x __temp0))
:__temp15
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- -1)
(__temp0 <- 1)
(__temp0 <- 11)
(__temp0 >>= 1)
(__temp16 <- (mem x 0))
(cjump __temp16 < __temp0 :__temp17 :__temp18)
:__temp17
(eax <- (array-error x __temp0))
:__temp18
(__temp0 *= 4)
(__temp0 += x)
((mem __temp0 4) <- 3)
(__temp0 <- 1)
(eax <- (print x))
(eax <- eax)))""")


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