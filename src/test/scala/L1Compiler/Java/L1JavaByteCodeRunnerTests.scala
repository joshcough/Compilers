package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaByteCodeRunnerTests extends FunSuite{
  
  val runner = L1JavaByteCodeRunner

  jvmTest("simple print"){
    assert(runner.test("""(((eax <- (print 5))))""") === "2")
  }

  jvmTest("print twice"){
    assert(runner.test(
      """
      ((
        (eax <- (print 5))
        (eax <- (print 5))
      ))""") === "2\n2")
  }

  jvmTest("allocate and then print"){
    assert(
      runner.test(
      """
      ((
        (eax <- (allocate 5 5))
        (eax <- (print 5))
      ))
      """) === "2")
  }

  jvmTest("put number in register"){
    assert(runner.test(
      """
      ((
        (ebx <- 7)
        (eax <- (print ebx))
      ))""") === "3")
  }

  jvmTest("mov register to register"){
    assert(runner.test(
      """
      ((
        (ebx <- 9)
        (ecx <- ebx)
        (eax <- (print ecx))
      ))""") === "4")
  }

  jvmTest("mov to register from memory"){
    assert(runner.test(
      """
      ((
        (eax <- (allocate 5 5))
        (esi <- (mem eax 4))
        (eax <- (print eax))
        (eax <- (print esi))
      ))""") === "{s:2, 2, 2}\n2")
  }

  def jvmTest(name:String)(f: => Unit) {
    test(name){
      try f
      catch { case e =>
        println("heap for '" + name + "': " + L1JavaRuntime.heapView)
        throw e
      }
    }
  }
}