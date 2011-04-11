package L2Compiler

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

  test("from homework"){
    // should result in: ((in (eax) (eax x)) (out (eax x) ()))
    val code = "((x <- 1) (eax += x))"
    val expectedAtEnd = """
      |((x <- 1) (eax) (eax x))
      |((eax += x) (eax x) ())"""
    livenessTest(code, expectedAtEnd, step=End)
  }


  def End = None // sort of hacky, but whatever.
  def Just(i:Int) = Some(i)
  lazy val count = Iterator.from(0)
  new java.io.File("./liveness-test").mkdir()
  def livenessTest(code:String, expected:String, step: Option[Int] = None) = {
    val insAndOuts = compiler.inoutForTesting(code.stripMargin.trim, step=step)
    val actual = insAndOuts.mkString("\n")
    if(actual.stripMargin.trim != expected.stripMargin.trim){
      println("failure!")
      println("code:\n" + code.stripMargin.trim)
      println("actual:\n" + actual.stripMargin.trim)
      println("expected:\n" + expected.stripMargin.trim)
    }
    assert(actual.stripMargin.trim === expected.stripMargin.trim)

    // print hw view only if we are viewing the final result of a liveness run
    if(!step.isDefined){
      // write out the tests files and results.
      import java.io.File
      import io.FileHelper._
      val index = count.next()
      // write the test
      new File("./liveness-test/test" + index + ".L2f").write(code.stripMargin.trim)
      // write the expected result
      new File("./liveness-test/test" + index + ".lres").write(InstructionInOutSet.hwView(insAndOuts))
    }
  }
}

