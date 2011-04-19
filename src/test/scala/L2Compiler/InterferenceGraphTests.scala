package L2Compiler

import L2AST._

class InterferenceGraphTests extends L2CompilerTest {

  test("base graph with just the registers") {
    assert(registerInterference.hwView === """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx))""".clean)
  }

  test("base graph with some added interference") {
    val x = Variable("x")
    assert(registerInterference.addInterference(eax -> x, edi -> x, esi -> x).hwView === """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x)
      |(x eax edi esi))""".clean)
  }

  interferenceAndAllocationTest(
    name = "http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture05.pdf (p 111)",
    code = """
      |(:f
      |(x2 <- eax)
      |(x2 *= x2)
      |(2x2 <- x2)
      |(2x2 *= 2)
      |(3x <- eax)
      |(3x *= 3)
      |(eax <- 2x2)
      |(eax += 3x)
      |(eax += 4)
      |(return))""",
    expectedInterference = """
      |((2x2 3x eax edi esi)
      |(3x 2x2 eax edi esi)
      |(eax 2x2 3x ebx ecx edi edx esi x2)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi 2x2 3x eax ebx ecx edx esi x2)
      |(edx eax ebx ecx edi esi)
      |(esi 2x2 3x eax ebx ecx edi edx x2)
      |(x2 eax edi esi))""",
    expectedAllocation="((2x2 ebx) (3x ecx) (x2 ebx))")

  interferenceAndAllocationTest(
    name = "http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p 24)",
    code = """
      |;; f(x) = let y = g(x)
      |;; in h(y+x) + y*5
      |(:f
      |(x <- eax) ;; save our argument
      |(call :g) ;; call g with our argument
      |(y <- eax) ;; save g's result in y
      |(eax += x) ;; compute h's arg
      |(call :h) ;; call h
      |(y5 <- y) ;; compute y*5 in y5, i
      |(y5 *= 5) ;; compute y*5 in y5, ii
      |(eax += y5) ;; add h's res to y*5
      |(return)) ;; and we're done""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x y y5)
      |(ebx eax ecx edi edx esi x y)
      |(ecx eax ebx edi edx esi x y)
      |(edi eax ebx ecx edx esi x y y5)
      |(edx eax ebx ecx edi esi x y)
      |(esi eax ebx ecx edi edx x y y5)
      |(x eax ebx ecx edi edx esi y)
      |(y eax ebx ecx edi edx esi x)
      |(y5 eax edi esi))""",
    expectedAllocation="#f")

  // spilling y
  interferenceAndAllocationTest(
    name = "http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p 53)",
    code = """
      |;; f(x) = let y = g(x)
      |;; in h(y+x) + y*5
      |(:f
      |(x <- eax) ;; save our argument
      |(call :g) ;; call g with our argument
      |((mem ebp -4) <- eax) ;; save g's result in y
      |(eax += x) ;; compute h's arg
      |(call :h) ;; call h
      |(y5 <- (mem ebp -4)) ;; compute y*5 in y5, i
      |(y5 *= 5) ;; compute y*5 in y5, ii
      |(eax += y5) ;; add h's res to y*5
      |(return)) ;; and we're done""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x y5)
      |(ebx eax ecx edi edx esi x)
      |(ecx eax ebx edi edx esi x)
      |(edi eax ebx ecx edx esi x y5)
      |(edx eax ebx ecx edi esi x)
      |(esi eax ebx ecx edi edx x y5)
      |(x eax ebx ecx edi edx esi)
      |(y5 eax edi esi))""",
    expectedAllocation="#f")

  // spilling y and x
  interferenceAndAllocationTest(
    name = "http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p 72)",
    code = """
      |;; f(x) = let y = g(x)
      |;; in h(y+x) + y*5
      |(:f
      |((mem ebp -8) <- eax) ;; save our argument
      |(call :g) ;; call g with our argument
      |((mem ebp -4) <- eax) ;; save g's result in y
      |(sx0 <- (mem ebp -8))
      |(eax += sx0) ;; compute h's arg
      |(call :h) ;; call h
      |(y5 <- (mem ebp -4)) ;; compute y*5 in y5, i
      |(y5 *= 5) ;; compute y*5 in y5, ii
      |(eax += y5) ;; add h's res to y*5
      |(return)) ;; and we're done""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi sx0 y5)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi sx0)
      |(edi eax ebx ecx edx esi sx0 y5)
      |(edx eax ebx ecx edi esi sx0)
      |(esi eax ebx ecx edi edx sx0 y5)
      |(sx0 eax ecx edi edx esi)
      |(y5 eax edi esi))""",
    expectedAllocation="((sx0 ebx) (y5 ebx))")

  // spilling z1 and z2
  interferenceAndAllocationTest(
    name = "http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p96)",
    code = """
      |(:f
      |((mem ebp -4) <- edi)
      |((mem ebp -8) <- esi)
      |(x <- eax)
      |(call :g)
      |(y <- eax)
      |(eax += x)
      |(call :h)
      |(y5 <- y)
      |(y5 *= 5)
      |(eax += y5)
      |(edi <- (mem ebp -4))
      |(esi <- (mem ebp -8))
      |(return))""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x y y5)
      |(ebx eax ecx edi edx esi x y)
      |(ecx eax ebx edi edx esi x y)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi x y)
      |(esi eax ebx ecx edi edx)
      |(x eax ebx ecx edx y)
      |(y eax ebx ecx edx x)
      |(y5 eax))""",
    expectedAllocation="((x edi) (y esi) (y5 ebx))")

  interferenceAndAllocationTest(
    name="http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/graph-test.pdf, problem 2",
    code = """
      |((r:x <- eax)
      |(r:x += ebx)
      |(r:x += ecx)
      |(r:x += edx)
      |(r:x += edi)
      |(r:x += esi)
      |(r:x += eax))""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi r:x)
      |(ebx eax ecx edi edx esi r:x)
      |(ecx eax ebx edi edx esi r:x)
      |(edi eax ebx ecx edx esi r:x)
      |(edx eax ebx ecx edi esi r:x)
      |(esi eax ebx ecx edi edx r:x)
      |(r:x eax ebx ecx edi edx esi))""",
    expectedAllocation="#f")

  interferenceAndAllocationTest(
    name="http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/graph-test.pdf, problem 1",
    code = "((x <- 1) (eax += x) (return))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x)
      |(x eax edi esi))""",
    expectedAllocation="((x ebx))")

  def interferenceAndAllocationTest(name:String, code:String, expectedInterference:String, expectedAllocation:String){
    test(name){
      val inouts = inoutForTesting(code.clean, step=End)
      val interference = buildInterferenceSet(inouts)
      val actualAllocation = printAllocation(attemptAllocation(inouts)._1)
      assert(interference.hwView === expectedInterference.clean)
      assert(actualAllocation === expectedAllocation)
    }
  }
}