package L2Compiler
import RegisterColorGraph._
import L2AST._

class LivenessTest extends L2CompilerTest {

  import compiler._

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
    val code = "(:f (x <- 1) (eax += x) (return))"
    val expectedAtEnd = """
      |(:f (eax edi esi) (eax edi esi))
      |((x <- 1) (eax edi esi) (eax edi esi x))
      |((eax += x) (eax edi esi x) (eax edi esi))
      |((return) (eax edi esi) ())"""
    livenessTest(code, expectedAtEnd, step=End)
  }


  def End = None // sort of hacky, but whatever.
  def Just(i:Int) = Some(i)
  def livenessTest(code:String, expected:String, step: Option[Int] = None) = {
    val actual = inoutForTesting(code.stripMargin.trim, step=step).mkString("\n")
    if(actual.stripMargin.trim != expected.stripMargin.trim){
      println("failure!")
      println("code:\n" + code.stripMargin.trim)
      println("actual:\n" + actual.stripMargin.trim)
      println("expected:\n" + expected.stripMargin.trim)
    }
    assert(actual.stripMargin.trim === expected.stripMargin.trim)


    // here is where i would print things 
  }
}

class InterferenceTest extends L2CompilerTest {

  import compiler._


  test("interference 1"){
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
      |(return))""".stripMargin.trim

    assert(interferingVariables(code) ===
            Set((Variable("3x"),Variable("2x2")), (Variable("2x2"),Variable("3x"))))
  }
//
//  test("interference 2"){
//        val code = """
//(((z1 <- ebx)
//(z2 <- edi)
//(z3 <- esi)
//(in <- edx)
//(call :g)
//(edx <- in)
//(g-ans <- eax)
//(call :h)
//(eax += g-ans)
//(ebx <- z1)
//(edi <- z2)
//(esi <- z3)
//(return)))
//"""
//    val interference = interferingVariables(code)
//    val expected = Set((Variable("z1"),Variable("z2")), (Variable("z2"),Variable("z1")),
//        (Variable("g-ans"),Variable("z2")), (Variable("z2"),Variable("g-ans")),
//        (Variable("z2"),Variable("in")), (Variable("in"),Variable("z2")),
//        (Variable("g-ans"),Variable("z1")), (Variable("z1"),Variable("g-ans")),
//        (Variable("z3"),Variable("z1")), (Variable("z1"),Variable("z3")),
//        (Variable("in"),Variable("z1")), (Variable("z1"),Variable("in")),
//        (Variable("z3"),Variable("z2")), (Variable("z2"),Variable("z3")),
//        (Variable("z3"),Variable("g-ans")), (Variable("g-ans"),Variable("z3")),
//        (Variable("z3"),Variable("in")), (Variable("in"),Variable("z3"))
//        )
//    assert(interference === expected)
//  }
//
//  test("legit graph coloring"){
//    val code = """
//(((x2 <- edx)
//(x2 *= x2)
//(2x2 <- x2)
//(2x2 *= 2)
//(3x <- edx)
//(3x *= 3)
//(eax <- 2x2)
//(eax += 3x)
//(eax += 4)
//(return)))
//"""
//    // this must be colorable.
//    val coloredGraph = attemptToColor(code).get
//    assert(coloredGraph.colorOf(Variable("x2")) === RED)
//    assert(coloredGraph.colorOf(Variable("2x2")) === RED)
//    assert(coloredGraph.colorOf(Variable("3x")) === GREEN)
//  }
//
//  test("uncolorable"){
//    val code = """
//            ((((mem ebp -4) <- edx)
//            (call :g)
//            (edx <- (mem ebp -4))
//            (g-ans <- eax)
//            (call :h)
//            (eax += g-ans)
//            (return)))"""
//    println(inout(code))
//    //println(attemptToColor(code))
//  }
//
//
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
    
    assert(LiveRange.print(liveRanges(inoutForTesting(code))) === """
      |((eax 10)
      |(y5 2)
      |(edi 10)
      |(edx 6)
      |(esi 10)
      |(ecx 6)
      |(x 3)
      |(y 3))""".stripMargin.trim)
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

    assert(LiveRange.print(liveRanges(inoutForTesting(code))) === """
      |((eax 11)
      |(y5 2)
      |(edi 11)
      |(edx 7)
      |(esi 11)
      |(ecx 7)
      |(ebp 8)
      |(sx0 1))""".stripMargin.trim)
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

    assert(LiveRange.print(liveRanges(inoutForTesting(code))) === """
      |((z2 10)
      |(eax 14)
      |(y5 2)
      |(edi 2) (edi 2)
      |(edx 8)
      |(esi 3) (esi 1)
      |(ecx 8)
      |(x 3)
      |(y 3)
      |(z1 10))""".stripMargin.trim)
  }



//  test("choose spill variable"){
//    val code = """
//(((z1 <- ebx)
//(z2 <- edi)
//(z3 <- esi)
//(in <- edx)
//(call :g)
//(edx <- in)
//(g-ans <- eax)
//(call :h)
//(eax += g-ans)
//(ebx <- z1)
//(edi <- z2)
//(esi <- z3)
//(return)))
//"""
//    assert(chooseSpillVar(liveRanges(inout(code))) === Some(Variable("z2")))
//  }

}
