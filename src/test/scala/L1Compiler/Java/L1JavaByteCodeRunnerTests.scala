package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaByteCodeRunnerTests extends FunSuite{
  
  val runner = L1JavaByteCodeRunner

  test("simple print"){
    assert(runner.test("""(((eax <- (print 5))))""") === "2")
  }

  test("print twice"){
    assert(runner.test(
      """
      ((
        (eax <- (print 5))
        (eax <- (print 5))
      ))""") === "2\n2")
  }

  test("allocate and then print"){
    assert(
      runner.test(
      """
      ((
        (eax <- (allocate 5 5))
        (eax <- (print 5))
      ))
      """) === "2")
  }

  test("put number in register"){
    assert(runner.test(
      """
      ((
        (ebx <- 7)
        (eax <- (print ebx))
      ))""") === "3")
  }

  test("mov register to register"){
    assert(runner.test(
      """
      ((
        (ebx <- 9)
        (ecx <- ebx)
        (eax <- (print ecx))
      ))""") === "4")
  }

}