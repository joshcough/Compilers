package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaByteCodeRunnerTests extends FunSuite{

  test("simple print"){
    assert(L1JavaByteCodeRunner.test("""(((eax <- (print 5))))""") === "2")
  }

  test("print twice"){
    assert(L1JavaByteCodeRunner.test(
      """
      ((
        (eax <- (print 5))
        (eax <- (print 5))
      ))""") === "2\n2")
  }

  test("allocate and then print"){
    assert(
      L1JavaByteCodeRunner.test(
      """
      ((
        (eax <- (allocate 5 5))
        (eax <- (print 5))
      ))
      """) === "2")
  }
}