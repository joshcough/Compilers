package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaByteCodeRunnerTests extends FunSuite{

  test("something"){
    println(L1JavaByteCodeRunner.test(
      """
      ((
        (eax <- (allocate 5 5))
        (eax <- (print 5))
      ))
      """))
  }

}