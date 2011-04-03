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

  jvmTest("label declaration (without use)"){
    assert(runner.test("((:my_label))") === "")
  }

  jvmTest("assignment with comparison"){
    assert(runner.test(
      """
      ((
        (esi <- 7)
        (edi <- 8)
        (eax <- esi < edi) ;; puts a 1 in eax.
        (eax <- (print eax)) ;; 1 is printed as 0 because of our number encoding scheme.
      ))""") === "0")
  }

  jvmTest("cjump on registers"){
    assert(runner.test(
      """
      ((
        (ecx <- 6)
        (edx <- 5)
        (cjump ecx < edx :keep_going :done)
        :keep_going
        (eax <- (print 9999999))
        :done
        (esi <- ecx)
        (edi <- edx)
        (eax <- (print edx))
        (ebx <- edi < esi)
        (eax <- (print ebx))
        (ebx <- edi <= esi)
        (eax <- (print ebx))
        (ebx <- esi < edi)
        (cjump ebx <= 0 :terminate :aint_gonna_happen)
        :aint_gonna_happen
        :terminate
        (eax <- (print 85))
      ))""") === "2\n0\n0\n42")
  }

  jvmTest("mem write"){
    assert(
      runner.test(
      """
      ((
        (eax <- (allocate 5 5))
        ((mem eax 4) <- 7)
        (eax <- (print eax))
      ))
      """) === "{s:2, 3, 2}")
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