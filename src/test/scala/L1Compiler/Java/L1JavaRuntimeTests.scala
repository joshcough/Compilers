package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaRuntimeTests extends FunSuite {

  import L1JavaRuntime._

  testPrint(5, 2)
  testPrint(3, 1)
  testPrint(1, 0)
  testPrint(-1, -1)
  testPrint(-3, -2)

  def runtimeTest(name:String)(f: => Unit) = test(name){ newProgram(); f }
  def testPrint(i:Int, expected: Int) = runtimeTest("print " + (i -> expected)){
    assert(valueOf(i) === expected.toString)
  }

  runtimeTest("alloc"){
    // allocate a size 10 array filled with 5's
    // 21 is 10 endcoded
    allocate(21, 5)
    assert(heapView === List(10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    assert(valueOf(0) === "{s:10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}")
  }

  runtimeTest("alloc twice"){
    // allocate a size 10 array filled with 5's
    // 21 is 10 endcoded
    assert(allocate(21, 5) === 0)
    assert(allocate(21, 7) === 11)
    assert(valueOf(0) === "{s:10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}")
    assert(heapView === List(10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7))
    assert(valueOf(22) === "{s:10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}")
  }

  runtimeTest("registers"){
    mov(eax, 85)
    mov(ebx, ":rwer")
    mov(ebx, eax)
    assert(valueOf(eax) === "42")
    assert(valueOf(ebx) === "42")
  }

  runtimeTest("read"){
    mov(eax, allocate(21, 5))
    mov(eax, allocate(21, 7))
    val size = read(eax, 0).asInstanceOf[Int]
    for(i <- 1 until size) assert(valueOf(read(eax, i * 4)) === "3")
  }

  runtimeTest("+="){
    mov(eax, 7)
    +=(eax, 2)
    assert(valueOf(eax) === "4")
    -=(eax, 4)
    assert(valueOf(eax) === "2")
    *=(eax, 3)
    assert(valueOf(eax) === "7")
    // TODO test &=
  }

  def valueOf(a:Any) = print(a).trim

}