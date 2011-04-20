package L2Compiler

import java.io.File
import io.FileHelper._

class LivenessTest extends L2CompilerTest {

  test("test first big example from lecture notes") {
    val code = """
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
      |(return))"""

    val expectedAfter1Step = """
      |(:f () ())
      |((x2 <- eax) (eax) ())
      |((x2 *= x2) (x2) ())
      |((2x2 <- x2) (x2) ())
      |((2x2 *= 2) (2x2) ())
      |((3x <- eax) (eax) ())
      |((3x *= 3) (3x) ())
      |((eax <- 2x2) (2x2) ())
      |((eax += 3x) (3x eax) ())
      |((eax += 4) (eax) ())
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAfter1Step, step=Just(1))

    val expectedAfter2Steps = """
      |(:f () (eax))
      |((x2 <- eax) (eax) (x2))
      |((x2 *= x2) (x2) (x2))
      |((2x2 <- x2) (x2) (2x2))
      |((2x2 *= 2) (2x2) (eax))
      |((3x <- eax) (eax) (3x))
      |((3x *= 3) (3x) (2x2))
      |((eax <- 2x2) (2x2) (3x eax))
      |((eax += 3x) (3x eax) (eax))
      |((eax += 4) (eax) (eax edi esi))
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAfter2Steps, step=Just(2))

    val expectedAtEnd = """
      |(:f (eax edi esi) (eax edi esi))
      |((x2 <- eax) (eax edi esi) (eax edi esi x2))
      |((x2 *= x2) (eax edi esi x2) (eax edi esi x2))
      |((2x2 <- x2) (eax edi esi x2) (2x2 eax edi esi))
      |((2x2 *= 2) (2x2 eax edi esi) (2x2 eax edi esi))
      |((3x <- eax) (2x2 eax edi esi) (2x2 3x edi esi))
      |((3x *= 3) (2x2 3x edi esi) (2x2 3x edi esi))
      |((eax <- 2x2) (2x2 3x edi esi) (3x eax edi esi))
      |((eax += 3x) (3x eax edi esi) (eax edi esi))
      |((eax += 4) (eax edi esi) (eax edi esi))
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAtEnd, step=End)
  }

  test("test call example from lecture notes") {
    val code = """
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
      |(return)) ;; and we're done"""

    val expectedAfter1Step = """
      |(:f () ())
      |((x <- eax) (eax) ())
      |((call :g) (eax ecx edx) ())
      |((y <- eax) (eax) ())
      |((eax += x) (eax x) ())
      |((call :h) (eax ecx edx) ())
      |((y5 <- y) (y) ())
      |((y5 *= 5) (y5) ())
      |((eax += y5) (eax y5) ())
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAfter1Step, step=Just(1))

    val expectedAfter2Steps = """
      |(:f () (eax))
      |((x <- eax) (eax) (eax ecx edx))
      |((call :g) (eax ecx edx) (eax))
      |((y <- eax) (eax) (eax x))
      |((eax += x) (eax x) (eax ecx edx))
      |((call :h) (eax ecx edx) (y))
      |((y5 <- y) (y) (y5))
      |((y5 *= 5) (y5) (eax y5))
      |((eax += y5) (eax y5) (eax edi esi))
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAfter2Steps, step=Just(2))

    val expectedAtEnd = """
      |(:f (eax ecx edi edx esi) (eax ecx edi edx esi))
      |((x <- eax) (eax ecx edi edx esi) (eax ecx edi edx esi x))
      |((call :g) (eax ecx edi edx esi x) (eax ecx edi edx esi x))
      |((y <- eax) (eax ecx edi edx esi x) (eax ecx edi edx esi x y))
      |((eax += x) (eax ecx edi edx esi x y) (eax ecx edi edx esi y))
      |((call :h) (eax ecx edi edx esi y) (eax edi esi y))
      |((y5 <- y) (eax edi esi y) (eax edi esi y5))
      |((y5 *= 5) (eax edi esi y5) (eax edi esi y5))
      |((eax += y5) (eax edi esi y5) (eax edi esi))
      |((return) (eax edi esi) ())"""

    livenessTest(code, expectedAtEnd, step=End)
  }

  // should result in: ((in (eax) (eax x)) (out (eax x) ()))
  test("from homework"){
    val code = "((x <- 1) (eax += x))"
    val expectedAtEnd = """
      |((x <- 1) (eax) (eax x))
      |((eax += x) (eax x) ())"""
    livenessTest(code, expectedAtEnd, step=End)
  }

  test("cjump"){
    val code = """
      |((x <- 0)
      |:label1
      |(x += 1)
      |(cjump x < 2 :label1 :label2)
      |:label2)"""
    val expectedAfter1 = """
      |((x <- 0) () ())
      |(:label1 () ())
      |((x += 1) (x) ())
      |((cjump x < 2 :label1 :label2) (x) ())
      |(:label2 () ())"""
    livenessTest(code, expectedAfter1, step=Just(1))

    val expectedAfter2 = """
      |((x <- 0) () ())
      |(:label1 () (x))
      |((x += 1) (x) (x))
      |((cjump x < 2 :label1 :label2) (x) ())
      |(:label2 () ())"""
    livenessTest(code, expectedAfter2, step=Just(2))

    val expectedAfter3 = """
      |((x <- 0) () ())
      |(:label1 (x) (x))
      |((x += 1) (x) (x))
      |((cjump x < 2 :label1 :label2) (x) ())
      |(:label2 () ())"""
    livenessTest(code, expectedAfter3, step=Just(3))

    // here is where the cjump gets x from the in of :label1
    val expectedAfter4 = """
      |((x <- 0) () (x))
      |(:label1 (x) (x))
      |((x += 1) (x) (x))
      |((cjump x < 2 :label1 :label2) (x) (x))
      |(:label2 () ())"""
    livenessTest(code, expectedAfter4, step=Just(4))

    val expectedAtEnd = """
      |((x <- 0) () (x))
      |(:label1 (x) (x))
      |((x += 1) (x) (x))
      |((cjump x < 2 :label1 :label2) (x) (x))
      |(:label2 () ())"""
    livenessTest(code, expectedAtEnd, step=End)
  }

  test("call"){
    livenessTest(
      "((call :fib))",
      "((call :fib) (eax ecx edx) ())",
      step=End)
  }

  test("tail-call"){
    livenessTest(
      "((tail-call :fib))",
      "((tail-call :fib) (eax ecx edi edx esi) ())",
      step=End)
  }

  test("return"){
    livenessTest(
      "((return))",
      "((return) (eax edi esi) ())",
      step=End)
  }

  test("call with args"){
    livenessTest(
      "((call x))",
      "((call x) (eax ecx edx x) ())",
      step=End)
  }

  test("tail-call with args"){
    livenessTest(
      "((tail-call x))",
      "((tail-call x) (eax ecx edi edx esi x) ())",
      step=End)
  }

  test("return with instruction after it..."){
    livenessTest(
      "(:label1 (return) :label2 (return))",
      """
      |(:label1 (eax edi esi) (eax edi esi))
      |((return) (eax edi esi) ())
      |(:label2 (eax edi esi) (eax edi esi))
      |((return) (eax edi esi) ())""",
      step=End)
  }

  test("call with args with instruction after it"){
    livenessTest(
      "((call x) (return))",
      """
      |((call x) (eax ecx edi edx esi x) (eax edi esi))
      |((return) (eax edi esi) ())""",
      step=End)
  }

  test("tail-call with args with instruction after it"){
    livenessTest(
      "((tail-call x) (return))",
      """
      |((tail-call x) (eax ecx edi edx esi x) ())
      |((return) (eax edi esi) ())""",
      step=End)
  }

  test("empty assignment"){
    livenessTest(
      "((eax <- 7))",
      "((eax <- 7) () ())",
      step=End)
  }

  test("simple assignment"){
    livenessTest(
      "((eax <- x))",
      "((eax <- x) (x) ())",
      step=End)
  }

  test("simple assignment with something after it"){
    livenessTest(
      "((eax <- x) (eax <- (print eax)))",
      """
        |((eax <- x) (x) (eax))
        |((eax <- (print eax)) (eax) ())""",
      step=End)
  }

  test("simple assignment with something after it 2"){
    livenessTest(
      "((eax <- x) (eax <- (print x)))",
      """
        |((eax <- x) (x) (x))
        |((eax <- (print x)) (x) ())""",
      step=End)
  }

  test("simple assignment with something after it 3"){
    livenessTest(
      """
        |((eax <- x)
        |(eax <- (print x))
        |(eax <- (print eax)))""",
      """
        |((eax <- x) (x) (x))
        |((eax <- (print x)) (x) (eax))
        |((eax <- (print eax)) (eax) ())""",
      step=End)
  }

  test("allocate"){
    livenessTest(
      "((eax <- (allocate eax x)) (eax <- (print x)))",
      """
        |((eax <- (allocate eax x)) (eax x) (x))
        |((eax <- (print x)) (x) ())""",
      step=End)
  }

  test("print doesnt kill ebx"){
    livenessTest(
      """
        |((eax <- 1)
        |(ebx <- 1)
        |(ecx <- 1)
        |(edx <- 1)
        |(edi <- 1)
        |(esi <- 1)
        |(eax <- (print 7))
        |(eax <- (print ebx)))""",
    """
        |((eax <- 1) () ())
        |((ebx <- 1) () (ebx))
        |((ecx <- 1) (ebx) (ebx))
        |((edx <- 1) (ebx) (ebx))
        |((edi <- 1) (ebx) (ebx))
        |((esi <- 1) (ebx) (ebx))
        |((eax <- (print 7)) (ebx) (ebx))
        |((eax <- (print ebx)) (ebx) ())""", step = End
    )
  }

  test("print doesnt kill ebx, edi, or esi"){
    livenessTest(
      """
        |((eax <- 1)
        |(ebx <- 1)
        |(ecx <- 1)
        |(edx <- 1)
        |(edi <- 1)
        |(esi <- 1)
        |(eax <- (print 7))
        |(eax <- (print ebx))
        |(eax <- (print edi))
        |(eax <- (print esi)))""",
    """
        |((eax <- 1) () ())
        |((ebx <- 1) () (ebx))
        |((ecx <- 1) (ebx) (ebx))
        |((edx <- 1) (ebx) (ebx))
        |((edi <- 1) (ebx) (ebx edi))
        |((esi <- 1) (ebx edi) (ebx edi esi))
        |((eax <- (print 7)) (ebx edi esi) (ebx edi esi))
        |((eax <- (print ebx)) (ebx edi esi) (edi esi))
        |((eax <- (print edi)) (edi esi) (esi))
        |((eax <- (print esi)) (esi) ())""", step = End
    )
  }

  test("goto"){
    livenessTest("""
        |((goto :keep_going)
        |:keep_going
        |(eax <- (print ebx))
        |(goto :end)
        |(eax <- (print ecx)) ; should not be printed
        |:end
        |(eax <- (print 7)))""",
      """
        |((goto :keep_going) (ebx) (ebx))
        |(:keep_going (ebx) (ebx))
        |((eax <- (print ebx)) (ebx) ())
        |((goto :end) () ())
        |((eax <- (print ecx)) (ecx) ())
        |(:end () ())
        |((eax <- (print 7)) () ())""",
      step=End)
  }

  test("infinite loop"){
    livenessTest("""
        |(:keep_going
        |(eax *= eax)
        |(goto :keep_going))""",
      """
        |(:keep_going (eax) (eax))
        |((eax *= eax) (eax) (eax))
        |((goto :keep_going) (eax) (eax))""",
      step=End)
  }

  test("array error has no successors"){
    livenessTest("""
        |((eax <- (array-error ebx ecx))
        |(eax -= eax))""",
      """
        |((eax <- (array-error ebx ecx)) (ebx ecx) ())
        |((eax -= eax) (eax) ())""",
      step=End)
  }

  test("cmp"){
    livenessTest(
      "((eax <- ebx < ecx))",
      "((eax <- ebx < ecx) (ebx ecx) ())",
      step=End)
  }

  test("mem read"){
    livenessTest(
      "((eax <- (mem ebx 4)))",
      "((eax <- (mem ebx 4)) (ebx) ())",
      step=End)
  }

  test("mem write"){
    livenessTest(
      "(((mem ebx 4) <- ecx)))",
      "(((mem ebx 4) <- ecx) (ebx ecx) ())",
      step=End)
  }

  lazy val count = Iterator.from(0)
  new java.io.File("./liveness-test").mkdir()
  def livenessTest(code:String, expected:String, step: Option[Int] = None) = {
    val insAndOuts = inoutForTesting(code.clean, step=step)
    val actual = L2Printer.testView(insAndOuts)
    if(actual.clean != expected.clean){
      println("code:\n" + code.clean)
      println("actual:\n" + actual.clean)
      println("expected:\n" + expected.clean)
    }
    assert(actual.clean === expected.clean)

    // print hw view only if we are viewing the final result of a liveness run
    if(!step.isDefined){
      // write out the tests files and results.

      val index = count.next()
      // write the test
      new File("./liveness-test/test" + index + ".L2f").write(code.clean)
      // write the expected result
      assert(LivenessMain.liveness(code.clean) === L2Printer.hwView(insAndOuts))
      new File("./liveness-test/test" + index + ".lres").write(L2Printer.hwView(insAndOuts))
    }
  }
}

class LiveRangeTests extends L2CompilerTest {
  // live range tests
  test("http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p24)"){
    val code = """
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
      |(return)) ;; and we're done""".stripMargin.trim

    assert(printLiveRanges(liveRanges(inoutForTesting(code))) === """
      |((eax 10)
      |(ecx 6)
      |(edi 10)
      |(edx 6)
      |(esi 10)
      |(x 3)
      |(y 3)
      |(y5 2))""".stripMargin.trim)
  }

  test("http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p73)"){
    val code = """
      |(:f
      |((mem ebp -8) <- eax)
      |(call :g)
      |((mem ebp -4) <- eax)
      |(sx0 <- (mem ebp -8))
      |(eax += sx0)
      |(call :h)
      |(y5 <- (mem ebp -4))
      |(y5 *= 5)
      |(eax += y5)
      |(return))""".stripMargin.trim

    assert(printLiveRanges(liveRanges(inoutForTesting(code))) === """
      |((eax 11)
      |(ecx 7)
      |(edi 11)
      |(edx 7)
      |(esi 11)
      |(sx0 1)
      |(y5 2))""".stripMargin.trim)
  }

  test("http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf (p95)"){
    val code = """
      |(:f
      |(z1 <- edi)
      |(z2 <- esi)
      |(x <- eax)
      |(call :g)
      |(y <- eax)
      |(eax += x)
      |(call :h)
      |(y5 <- y)
      |(y5 *= 5)
      |(eax += y5)
      |(edi <- z1)
      |(esi <- z2)
      |(return))""".stripMargin.trim

    assert(printLiveRanges(liveRanges(inoutForTesting(code))) === """
      |((eax 14)
      |(ecx 8)
      |(edi 2) (edi 2)
      |(edx 8)
      |(esi 3) (esi 1)
      |(x 3)
      |(y 3)
      |(y5 2)
      |(z1 10)
      |(z2 10))""".stripMargin.trim)
  }
}
