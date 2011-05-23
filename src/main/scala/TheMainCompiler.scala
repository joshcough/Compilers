import L2Compiler.{L2Printer, L2Compiler}
import L3Compiler.{L3Printer, L3Compiler}
import L4Compiler.{L4Printer, L4Compiler}
import L5Compiler.{L5CompilerTests, L5Compiler}
import util.{L2Interpreter, L3Interpreter, L4Interpreter, L1Interpreter, L5Interpreter, TestHelpers}

case class CompilationResult(l4:String, l3:String, l2:String, l1:String)

object TheMainCompiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String) = compileToStrings(new File(filename).read)
  def compileToStrings(code:String): (String, String, String, String) = {
    val l5c = new L5Compiler{}
    val l4c = new L4Compiler{}
    val l3c = new L3Compiler{}
    val l2c = new L2Compiler{}

    val l4 = l5c.compile(code)
//    println("l4: " + L4Printer.toCode(l4))
    val l3 = l4c.compile(l4)
//    println("l3: " + l3)
    val l2 = l3c.compile(l3)
//    println("l2: " + L2Printer.toCode(l2))
    val l1String = L2Printer.toCode(l2c.compile(l2))
//    println("l1: " + l1String)
    val result = (L4Printer.toCode(l4), L3Printer.toCode(l3), L2Printer.toCode(l2), l1String)
    println("done compiling!")
    result
  }
}

class TheMainCompilerTests extends TestHelpers {

  testCompile("(test 5 5)")
//  testCompile("(test (identity 14) 14)")
//  testCompile("(test false false)")
//  testCompile("(test true true)")
//  testCompile("(test true (not false))")
//  testCompile("(test false (not true))")
//  testCompile("(test (and true true) true)")
//  testCompile("(test (and true false) false)")
//  testCompile("(test (and false true) false)")
//  testCompile("(test (and false false) false)")
//  testCompile("(test (or true true) true)")
//  testCompile("(test (or true false) true)")
//  testCompile("(test (or false true) true)")
//  testCompile("(test (or false false) false)")
//  testCompile("(test (+ 5 5) 10)")

  import io.FileHelper._
  new java.io.File("./test/the-test").mkdir()
  lazy val testcount = Iterator.from(0)

//  def wrapLibrary(code:String) = """
//  (let ([identity (lambda (x) x)])
//
//  ;;;;;;;;;;;;;;;;;;
//  ;; Booleans
//  ;;;;;;;;;;;;;;;;;;
//  (let ([true 1])
//  (let ([false 0])
//  (let ([not (lambda (x) (if (= true x) false true))])
//  (let ([and (lambda (x y) (if (= false x) false (if (= false y) false true)))])
//  (let ([or  (lambda (x y) (if (= false x) (if (= false y) false true) true))])
//
//  ;;;;;;;;;;;;;;;;;;
//  ;; Test Functions
//  ;;;;;;;;;;;;;;;;;;
//  (let ([eq (lambda (x y) (= x y))])
//  (let ([test (lambda (x y) (if (eq x y) (print 1) (begin (print x) (print y))))])
//
//""" + code + """
//  ))))))))
//"""

  def wrapLibrary(code:String) = """
  ;;;;;;;;;;;;;;;;;;
  ;; Test Functions
  ;;;;;;;;;;;;;;;;;;
  ;(let ([eq (lambda (x y) (= x y))])
  (let ([test (lambda (x y) (if (= x y) (print 1) (begin (print x) (print y))))])
""" + code + """
  )
"""

  def testCompile(L5E:String) = {
    val index = testcount.next()
    test(index + "-" + L5E){
      val l5 = wrapLibrary(L5E.clean)
      //val l5 = L5E.clean
      val (l4, l3, l2, l1) = TheMainCompiler.compileToStrings(l5)
      // write the test
      new java.io.File("./test/the-test/test" + index + ".L5").write(l5)
      new java.io.File("./test/the-test/test" + index + ".L4").write(l4)
      new java.io.File("./test/the-test/test" + index + ".L3").write(l3)
      new java.io.File("./test/the-test/test" + index + ".L2").write(l2)
      new java.io.File("./test/the-test/test" + index + ".L1").write(l1)

      def die(level:String, otherResult:String, codes:String*){
        codes.foreach{c => println(c); println(); println()}
        fail(level + "\nother result: " + otherResult)
      }

      val L5InterpResult = L5Interpreter.run(l5)
      val L1InterpResult = L1Interpreter.run(l1)

      verboseAssert(L5E, L5InterpResult, "1")
      if(L5InterpResult != L1InterpResult) {
        val L4InterpResult = L4Interpreter.run(l4)
        if(L5InterpResult == L4InterpResult) {
          val L3InterpResult = L3Interpreter.run(l3)
          if(L5InterpResult == L3InterpResult) {
            val L2InterpResult = L2Interpreter.run(l2)
            if(L5InterpResult == L2InterpResult) {
              die("L5 != L1", L1InterpResult, l5, l4, l3, l2, l1)
            }
            else die("L5 != L2", L2InterpResult, l5, l4, l3, l2)
          }
          else die("L5 != L3", L3InterpResult, l5, l4, l3)
        }
        else die("L5 != L4", L4InterpResult, l5, l4)
      }
    }
  }
}