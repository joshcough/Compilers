package L1Compiler.Java

import org.scalatest.FunSuite

class L1JavaRuntimeTests extends FunSuite {

  testPrint(5, 2)
  testPrint(3, 1)
  testPrint(1, 0)
  testPrint(-1, -1)
  testPrint(-3, -2)

  def testPrint(i:Int, expected: Int) = {
    test("print " + (i -> expected)){
      assert(new L1JavaRuntime().print(i).trim === expected.toString)
    }
  }

  test("alloc"){
    val runtime = new L1JavaRuntime()
    // allocate a size 10 array filled with 5's
    // 21 is 10 endcoded
    runtime.allocate(21, 5)
    assert(runtime.heapView === List(10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    assert(runtime.print(0).trim === "{s:10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}")
  }

  test("alloc twice"){
    val runtime = new L1JavaRuntime()
    // allocate a size 10 array filled with 5's
    // 21 is 10 endcoded
    assert(runtime.allocate(21, 5) === 0)
    assert(runtime.allocate(21, 7) === 11)
    assert(runtime.print(0).trim === "{s:10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}")
    assert(runtime.heapView === List(10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7))
    assert(runtime.print(22).trim === "{s:10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}")
  }
}