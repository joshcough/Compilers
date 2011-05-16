package L3Compiler

import util.{L2Interpreter, L3Interpreter, TestHelpers}

class L3CompilerTests extends TestHelpers with util.SlowTest {

  testCompile("((print 7))")
  testCompile("((:f 10) (:f (x) (print x)))")
  testCompile("((let ([x 7]) (print x)))")
  testCompile("((:f 10) (:f (x) (let ([y 7]) (print y))))")
  testCompile("((+ 7 10))", "(((eax <- 15) (eax += 21) (eax -= 1)))")
  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (+ x y)]) (print z)))))")
  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (+ x y)]) z))))")
  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (- x y)]) (print z)))))")
  testCompile("((:f 10) (:f (x) (let ([y 7]) (let ([z (* x y)]) (print z)))))")
  testCompile("((let ([x :f]) (x 5)) (:f (x) (print x)))")
  testCompile("((let ([x (< 5 6)]) (print x)))")
  testCompile("((let ([x (new-array 5 5)]) (print x)))")
  testCompile("((let ([x (new-array 7 5)]) (let ([y (aset x 1 3)]) (print x))))")
  testCompile("((let ([x (new-array 7 5)]) (let ([y (alen x)]) (print y))))")
  testCompile("((let ([x (new-array 7 5)]) (let ([y (aref x 2)]) (print y))))")
  testCompile("((if 0 (print 5) (print 6)))")
  testCompile("""
  ((let ([answer (:fib 10)]) (print answer))
   (:fib (x)
     (let ([xlt2 (< x 2)])
       (if xlt2
         1
         (let ([x1 (- x 1)])
           (let ([f1 (:fib x1)])
             (let ([x2 (- x 2)])
               (let ([f2 (:fib x2)])
                 (+ f1 f2)))))))))""")

  testCompile("((let ([x_0 (new-array 3 7)]) (let ([x_2 (a? x_0)]) (if x_2 (print x_2) (print x_2)))))")
  testCompile("((let ([x_0 (new-array 3 7)]) (let ([x_2 (a? x_0)]) (if x_2 (print 1) (print 0)))))")
  testCompile("((let ([x_0 (new-array 3 7)]) (let ([x_1 (* 4 2)]) (let ([x_2 (a? x_0)]) (if x_2 (print 1) (print 0))))))")
  testCompile("((let ([x_0 (new-array 3 7)]) (let ([x_2 (number? x_0)]) (if x_2 (print 1) (print 0)))))")
  testCompile("((let ([x_0 (new-array 3 7)]) (let ([x_1 (* 4 2)]) (let ([x_2 (number? x_0)]) (if x_2 (print 1) (print 0))))))")
  testCompile("((let ([x_0 7]) (let ([x_2 (a? x_0)]) (if x_2 (print 1) (print 0)))))")
  testCompile("((let ([x_0 7]) (let ([x_1 (* 4 2)]) (let ([x_2 (a? x_0)]) (if x_2 (print 1) (print 0))))))")
  testCompile("((let ([x_0 7]) (let ([x_2 (number? x_0)]) (if x_2 (print 1) (print 0)))))")
  testCompile("((let ([x_0 7]) (let ([x_1 (* 4 2)]) (let ([x_2 (number? x_0)]) (if x_2 (print 1) (print 0))))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 0))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 1))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 9))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 10))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 11))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 20))) (print v))))")
  testCompile("((let ((a (new-array 10 1))) (let ((v (aref a 100))) (print v))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 0 9)]) (print a)))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 1 9)]) (print a)))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 2 9)]) (print a)))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 0 9)]) (let ((v (aref a -1))) (print v))))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 0 9)]) (let ((v (aref a 0))) (print v))))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 1 9)]) (let ((v (aref a 1))) (print v))))))")
  testCompile("((let ([a (new-array 3 7)]) (let ([b (aset a 2 9)]) (let ((v (aref a 2))) (print v))))))")
  testCompile("""((let ((t (new-tuple 1))) (print t)))""")
  testCompile("((let ([x (new-tuple 3)]) (print x)))")
  testCompile("((let ((t (new-tuple 1 2 3))) (print t)))")
  testCompile("((let ([x (new-tuple 3 29 0 -5 -1 1)]) (print x)))")
  testCompile("""
  ((let ((t (new-array 1 2)))
    (let ((c (make-closure :f t)))
      (let ((a (closure-vars c)))
        (print a))))
  (:f (x) x))""")
  testCompile("""
  ((let ((t (new-array 1 2)))
    (let ((c (make-closure :f t)))
      (let ((a (closure-vars c)))
        (let ((v (aref a 0))) (print v)))))
  (:f (x) x))""")
  testCompile("""
  ((let ((t (new-tuple 1)))
    (let ((c (make-closure :f t)))
      (let ((a (closure-vars c)))
        (let ((v (aref a 0))) (print v)))))
  (:f (x) x))""")
  testCompile("""
  ((let ((t (new-tuple 1 2 3)))
    (let ((c (make-closure :f t)))
      (let ((a (closure-vars c)))
        (let ((v (aref a 0))) (print v)))))
  (:f (x) x))""")
  testCompile("""
((let ([args (new-tuple 4 5 6)])
(let ([clos (make-closure :myFun args)])
(let ([z (closure-proc clos)])
(let ([ans (z 4 5 6)])
(print ans)))))
(:myFun (x y q) (let ([z (+ x y)]) (+ z q))))
""")

  // two failures i had:
  testCompile("""((let ([a (new-array 10 0)])
    (let ([c 0])
      (:filla a c)))
   (:filla (a c)
    (let ([cg10 (< 9 c)])
      (if cg10
        (print a)
        (let ([cp1 (+ c 1)])
          (let ([newa (aset a c cp1)])
      (:filla a cp1)))))))""")

  testCompile("""((let ((n_1 10))
     (let ((a_2 (new-array n_1 0)))
       (let ((x_7 (- n_1 1)))
         (let ((__3 (:fill_and_print a_2 x_7))) (print a_2)))))
  (:fill_and_print
    (a_4 n_5)
    (let ((x_8 (= n_5 0)))
      (if x_8
        0
        (let ((__6 (aset a_4 n_5 n_5)))
          (let ((x_9 (- n_5 1))) (:fill_and_print a_4 x_9)))))))""")
  
  import io.FileHelper._
  new java.io.File("./test/3-test").mkdir()
  val testcount = Iterator.from(0)

  def testCompile(l3Code:String, expected:String="na") = {
    test(l3Code.clean){
      val compiler = new L3Compiler()
      import compiler._
      val l2Code = L2Compiler.L2Printer.toCode(compile(l3Code.clean))
//      println(l3Code.clean)
//      println("L2 Code: " + l2Code.clean)
//      verboseAssert(l3Code, read(l2Code.clean).toString, read(expected.clean).toString)
      val l3InterpResult = L3Interpreter.run(l3Code.clean)
      val l2InterpResult = L2Interpreter.run(l2Code.clean)
//      println("l3InterpResult: "+ l3InterpResult)
//      println("l2InterpResult: "+ l2InterpResult)
      verboseAssert("l3 vs l2 interps", l3InterpResult, l2InterpResult)

      // write out the tests files and results.
      val index = testcount.next()
      // write the test
      new java.io.File("./test/3-test/test" + index + ".L3").write(l3Code.clean)
      // write the expected result
      new java.io.File("./test/3-test/test" + index + ".L2").write(l2Code.clean)
    }
  }
}