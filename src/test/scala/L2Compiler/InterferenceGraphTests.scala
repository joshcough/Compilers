package L2Compiler

import L2AST._

class InterferenceGraphTests extends L2CompilerTest {

  interferenceAndAllocationTest(
    name = "no code at all",
    code = "()",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx))""",
    expectedAllocation="()")

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

  interferenceAndAllocationTest(
    name="simple right shift",
    code = "((x >>= x))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi x)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi x)
      |(esi eax ebx ecx edi edx x)
      |(x eax ebx edi edx esi))""",
    expectedAllocation="((x ecx))")

  interferenceAndAllocationTest(
    name="simple left shift",
    code = "((x <<= x))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi x)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi x)
      |(esi eax ebx ecx edi edx x)
      |(x eax ebx edi edx esi))""",
    expectedAllocation="((x ecx))")

  interferenceAndAllocationTest(
    name="right and left shifts",
    code = "((x <<= x) (x >>= x))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi x)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi x)
      |(esi eax ebx ecx edi edx x)
      |(x eax ebx edi edx esi))""",
    expectedAllocation="((x ecx))")

  interferenceAndAllocationTest(
    name="right and left shifts with two variables",
    code = "((x <- 7) (y <- 7) (x <<= y) (y <<= x) (x >>= y) (y >>= x))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi x y)
      |(ebx eax ecx edi edx esi x y)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x y)
      |(edx eax ebx ecx edi esi x y)
      |(esi eax ebx ecx edi edx x y)
      |(x eax ebx edi edx esi y)
      |(y eax ebx edi edx esi x))""",
    expectedAllocation="#f")

  interferenceAndAllocationTest(
    name="Comp 1",
    code = "((x <- 5 < 6))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x)
      |(x edi esi))""",
    expectedAllocation="((x eax))")

  interferenceAndAllocationTest(
    name="Comp 2",
    code = "((x <- 5 < 6) (y <- 5 <= 6) )",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x y)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x y)
      |(x edi esi)
      |(y edi esi))""",
    expectedAllocation="((x eax) (y eax))")

  interferenceAndAllocationTest(
    name="Comp 3",
    code = "((x <- 5 < 6) (y <- 5 <= 6) (x <- x < x))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x y)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x y)
      |(x edi esi y)
      |(y edi esi x))""",
    expectedAllocation="((x eax) (y ebx))")

  interferenceAndAllocationTest(
    name="x <- y with some usages",
    code = "((x <- y) (eax <- (print x)))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx)
      |(x)
      |(y))""",
    expectedAllocation="((x eax) (y eax))")

  interferenceAndAllocationTest(
    name="x <- y with some usages 2",
    code = "((x <- y) (eax <- (print y)))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx)
      |(x)
      |(y))""",
    expectedAllocation="((x eax) (y eax))")

  interferenceAndAllocationTest(
    name="x <- y",
    code = "((x <- y))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx)
      |(x)
      |(y))""",
    expectedAllocation="((x eax) (y eax))")

  interferenceAndAllocationTest(
    name="variable that conflicts with nothing",
    code = "((x <- 1) (eax <- (print x)))",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx)
      |(x))""",
    expectedAllocation="((x eax))")

  interferenceAndAllocationTest(
    name="robby email",
    code = """
      |((x <- 30)
      |(cjump x <= 31 :first :second)
      |:first
      |(x >>= 1)
      |(cjump x <= 15 :third :fourth)
      |:second
      |(x <- 4 < 5)
      |(goto :end)
      |:third
      |(goto :end)
      |:fourth
      |(x <- 5 < 4)
      |:end
      |(eax <- (print x)))""",
    expectedInterference = """
      |((eax ebx ecx edi edx esi)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x)
      |(x edi esi))""",
    expectedAllocation="((x eax))")

  interferenceAndAllocationTest(
    name="2010 thing",
    code = """
      |((a <- 1)
      |(b <- 2)
      |(c <- 3)
      |(d <- 4)
      |(e <- 5)
      |(f <- 6)
      |(g <- 7)
      |(h <- 8)
      |(a <<= h)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= g)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= f)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= e)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= d)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= c)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= b)
      |(a -= 50)
      |(a >>= a)
      |(a += 1)
      |(eax <- (print a)))""",
  // h doesnt intefere with ecx
  // the other variables do because the are live when print is called
  // and print kills ecx.
    expectedInterference = """
((a b c d e eax ebx ecx edi edx esi f g h)
(b a c d e eax ebx ecx edi edx esi f g h)
(c a b d e eax ebx ecx edi edx esi f g h)
(d a b c e eax ebx ecx edi edx esi f g h)
(e a b c d eax ebx ecx edi edx esi f g h)
(eax a b c d e ebx ecx edi edx esi f g h)
(ebx a b c d e eax ecx edi edx esi f g h)
(ecx a b c d e eax ebx edi edx esi f g)
(edi a b c d e eax ebx ecx edx esi f g h)
(edx a b c d e eax ebx ecx edi esi f g h)
(esi a b c d e eax ebx ecx edi edx f g h)
(f a b c d e eax ebx ecx edi edx esi g h)
(g a b c d e eax ebx ecx edi edx esi f h)
(h a b c d e eax ebx edi edx esi f g))""",
    expectedAllocation="#f")

  interferenceAndAllocationTest(
    name="first failure from robbys tests",
    code="""
      |((eax <- s1)
      |(eax <- s0)
      |(eax <- x1)
      |(eax <- x0)
      |(x0 <- s0)
      |(s1 <- 0)
      |(s0 <- 1)
      |(x1 <- 2)
      |(x0 <- 3))""",
    expectedInterference = """((eax ebx ecx edi edx esi)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi)
(edx eax ebx ecx edi esi)
(esi eax ebx ecx edi edx)
(s0 s1 x0 x1)
(s1 s0 x0 x1)
(x0 s0 s1 x1)
(x1 s0 s1 x0))""",
    expectedAllocation = "((s0 eax) (s1 ebx) (x0 ecx) (x1 edi))"
  )

  interferenceAndAllocationTest(
    name="jin/12",
    code="""(:fib
  (esp -= 12)
  (cjump eax < 2 :base :recur)
  :base
  (x <- 1)
  (eax <- x)
  (esp += 12)
  (return)
  :recur
  ((mem ebp -4) <- eax)
  (x <- 1)
  (eax -= x)
  (call :fib)
  ((mem ebp -8) <- eax)
  (eax <- (mem ebp -4))
  (x <- 2)
  (eax -= x)
  (return))""",
    expectedInterference="""
((eax ebx ecx edi edx esi x)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi x)
(edi eax ebx ecx edx esi x)
(edx eax ebx ecx edi esi x)
(esi eax ebx ecx edi edx x)
(x eax ecx edi edx esi))""",
    expectedAllocation="((x ebx))"
  )

  interferenceAndAllocationTest(name="x",
  code="((abc <- 11) (abc <- 11) (x0 <<= num) (x1 >>= num))",
  expectedInterference="""
((abc num x0 x1)
(eax ebx ecx edi edx esi num)
(ebx eax ecx edi edx esi num)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi num)
(edx eax ebx ecx edi esi num)
(esi eax ebx ecx edi edx num)
(num abc eax ebx edi edx esi x0 x1)
(x0 abc num x1)
(x1 abc num x0))
""",
  expectedAllocation = "((abc eax) (num ecx) (x0 ebx) (x1 edi))")

  new java.io.File("./graph-test").mkdir()

  def interferenceAndAllocationTest(name:String, code:String,
                                    expectedInterference:String,
                                    expectedAllocation:String,
                                    dumpAll:Boolean=false){
    test(name){

      val inouts = inout(parseListOfInstructions(code.clean))
      val alloc = attemptAllocation(inouts.head)
      val (interference, actualAllocation) =
        (buildInterferenceSet(inouts.head).hwView, printAllocation(alloc._1))

      if(dumpAll){
        for(io <- inouts.reverse) {
          println(L2Printer.hwView(io))
        }
      }

      //val (interference, actualAllocation) = InterferenceMain.interferenceAndAllocation(code.clean)
      verboseAssert(code, interference, expectedInterference)
      verboseAssert(code, actualAllocation, expectedAllocation)
      // write out the tests files and results.
      import java.io.File
      import io.FileHelper._
      val index = count.next()
      // write the test
      new File("./graph-test/test" + index + ".L2f").write(code.clean)
      // write the expected result
      new File("./graph-test/test" + index + ".gres").write(interference + "\n" + actualAllocation)
    }
  }

  test("base graph with some added interference") {
    val x = Variable("x")
    assert(registerInterference.addEdges(eax -> x, edi -> x, esi -> x).hwView === """
      |((eax ebx ecx edi edx esi x)
      |(ebx eax ecx edi edx esi)
      |(ecx eax ebx edi edx esi)
      |(edi eax ebx ecx edx esi x)
      |(edx eax ebx ecx edi esi)
      |(esi eax ebx ecx edi edx x)
      |(x eax edi esi))""".clean)
  }
}