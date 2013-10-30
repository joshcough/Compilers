package LCompiler

import util.Timer
import util.{L2Interpreter, L3Interpreter, L4Interpreter, L1Interpreter, L5Interpreter, TestHelpers}

class SpeedTest extends TestHelpers with Timer {

  testCompile("(print (stake 15 (fibs)))",
    expected="{s:2, 0, {s:2, 1, {s:2, 1, {s:2, 2, {s:2, 3, {s:2, 5, {s:2, 8, {s:2, 13, {s:2, 21, {s:2, 34, {s:2, 55, {s:2, 89, {s:2, 144, {s:2, 233, {s:2, 377, {s:0}}}}}}}}}}}}}}}}")

  def wrapLibrary(code:String) = """

  ;;;;;;;;;;;;;;;;;;
  ;; Lists
  ;;;;;;;;;;;;;;;;;;
  (let ([nil (new-tuple)])
  (let ([cons (lambda (x y) (new-tuple x y))])
  (let ([head (lambda (xs) (aref xs 0))])
  (let ([tail (lambda (xs) (aref xs 1))])

  ;;;;;;;;;;;;;;;;;;
  ;; Streams
  ;;;;;;;;;;;;;;;;;;
  (let ([scons (lambda (x y) (cons x y))])
  (let ([shead (lambda (s) (head s))])
  (let ([stail (lambda (s) ((tail s)))])

  (letrec ([stake (lambda (n s) (if (= n 0) nil (cons (shead s) (stake (- n 1) (stail s)))))])
  (letrec ([szipwith (lambda (f s1 s2)
    (scons (f (shead s1) (shead s2)) (lambda () (szipwith f (stail s1) (stail s2))))
  )])

  (letrec ([fibs (lambda ()
    (scons 0 (lambda () (scons 1 (lambda () (szipwith + (fibs) (stail (fibs)))))))
  )])
""" + code + """
  ))))))))))
"""


  import io.FileHelper._
  new java.io.File("./test/the-test").mkdir()
  lazy val testcount = Iterator.from(0)

  def testCompile(L5E:String, expected: String){
    val index = testcount.next()
    test(index + "-" + L5E){
      val l5 = wrapLibrary(L5E.clean)
      val List(l4, l3, l2, l1) = alwaysTimed("compiling", LCompiler.compileToStrings(l5))

      // write the test
      new java.io.File("./test/the-test/test" + index + ".L5").write(l5)
      new java.io.File("./test/the-test/test" + index + ".L4").write(l4)
      new java.io.File("./test/the-test/test" + index + ".L3").write(l3)
      new java.io.File("./test/the-test/test" + index + ".L2").write(l2)
      new java.io.File("./test/the-test/test" + index + ".L1").write(l1)

      def die(level:String, otherResult:String, codes:String*){
        println(codes.mkString("\n============\n"))
        fail(level + "\nother result: " + otherResult)
      }

      val L5InterpResult = L5Interpreter.run(l5)
      verboseAssert(L5E, L5InterpResult, expected)

//      val L1InterpResult = L1Interpreter.run(l1)
//
//      if(L5InterpResult != L1InterpResult) {
//        val L4InterpResult = L4Interpreter.run(l4)
//        if(L5InterpResult == L4InterpResult) {
//          val L3InterpResult = L3Interpreter.run(l3)
//          if(L5InterpResult == L3InterpResult) {
//            val L2InterpResult = L2Interpreter.run(l2)
//            if(L5InterpResult == L2InterpResult) {
//              die("L5 != L1", L1InterpResult, l5, l4, l3, l2, l1)
//            }
//            else die("L5 != L2", L2InterpResult, l5, l4, l3, l2)
//          }
//          else die("L5 != L3", L3InterpResult, l5, l4, l3)
//        }
//        else die("L5 != L4", L4InterpResult, l5, l4)
//      }
    }
  }
}