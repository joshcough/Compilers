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

  def End = None // sort of hacky, but whatever.
  def Just(i:Int) = Some(i)
  def livenessTest(code:String, expected:String, step: Option[Int] = None) = {
    val actual = inoutForTesting(code.stripMargin.trim, step=step)
    if(actual.stripMargin.trim != expected.stripMargin.trim){
      println("failure!")
      println("code:\n" + code.stripMargin.trim)
      println("actual:\n" + actual.stripMargin.trim)
      println("expected:\n" + expected.stripMargin.trim)
    }
    assert(actual.stripMargin.trim === expected.stripMargin.trim)
  }

//  test("from homework"){
//    // should result in:
//    // ((in (eax) (eax x)) (out (eax x) ()))
//    val code = "((x <- 1) (eax += x)) x -4 s"
//    assert(inout(code) === List(
//      InstuctionInOutSet(
//        Assignment(Variable("x"),Num(1)),
//          Set(eax),
//          Set(eax, Variable("x"))),
//      InstuctionInOutSet(
//        Increment(eax,Variable("x")),
//          Set(eax, Variable("x")),
//          Set())))
//  }

//  test("call"){
//val code = """
//(((in <- edx)
//(call :g)
//(edx <- in)
//(g-ans <- eax)
//(call :h)
//(eax += g-ans)
//(return)))"""
//    assert(inout(code) === List(
//      // :f
//      // (edx)                   (ebx edi edx esi)
//      InstuctionInOutSet(LabelDeclaration(Label("main")),
//        Set(edx),
//        Set(edx, edi, esi, ebx)),
//      // (in <- edx)
//      // (ebx edi edx esi)       (ebx edi esi in)
//      InstuctionInOutSet(Assignment(Variable("in"),edx),
//        Set(edx, edi, esi, ebx),
//        Set(Variable("in"), edi, esi, ebx)),
//      // (call :g)
//      // (ebx edi esi in)        (eax ebx edi esi in)
//      InstuctionInOutSet(Call(Label("g")),
//        Set(Variable("in"), edi, esi, ebx),
//        Set(eax, Variable("in"), edi, esi, ebx)),
//      //  (edx <- in)
//      //  (eax ebx edi esi in)    (eax ebx edi esi)
//      InstuctionInOutSet(Assignment(edx,Variable("in")),
//        Set(eax, Variable("in"), edi, esi, ebx),
//        Set(eax, ebx, edi, esi)),
//      // (g-ans <- eax)
//      // (eax ebx edi esi)       (ebx edi esi g-ans)
//      InstuctionInOutSet(Assignment(Variable("g-ans"),eax),
//        Set(eax, ebx, edi, esi),
//        Set(ebx, edi, esi, Variable("g-ans"))),
//      // (call :h)
//      // (ebx edi esi g-ans)     (eax ebx edi esi g-ans)
//      InstuctionInOutSet(Call(Label("h")),
//        Set(ebx, edi, esi, Variable("g-ans")),
//        Set(ebx, eax, edi, esi, Variable("g-ans"))),
//      // (eax += g-ans)
//      // (eax ebx edi esi g-ans) (ebx edi esi)
//      InstuctionInOutSet(Increment(eax,Variable("g-ans")),
//        Set(ebx, eax, edi, esi, Variable("g-ans")),
//        Set(ebx, edi, esi)),
//      // (return)
//      // (ebx edi esi)           ()
//      InstuctionInOutSet(Return,
//        Set(ebx, edi, esi),
//        Set())))
//  }
//
//  test("more"){
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
//    assert(inout(code) === List(
//      // :main
//      //    (edx)                (ebx edi edx esi)
//      InstuctionInOutSet(LabelDeclaration(Label("main")),
//        Set(edx),
//        Set(ebx, edi, esi, edx)),
//      // (z1 <- ebx)
//      //    (ebx edi edx esi)    (edi edx esi z1)
//      InstuctionInOutSet(Assignment(Variable("z1"),ebx),
//        Set(ebx, edi, esi, edx),
//        Set(edi, esi, edx, Variable("z1"))),
//      // (z2 <- edi)
//      //    (edi edx esi z1)     (edx esi z1 z2)
//      InstuctionInOutSet(Assignment(Variable("z2"),edi),
//        Set(edi, esi, edx, Variable("z1")),
//        Set(esi, edx, Variable("z1"), Variable("z2"))),
//      // (z3 <- esi)
//      //    (edx esi z1 z2)      (edx z1 z2 z3)
//      InstuctionInOutSet(Assignment(Variable("z3"),esi),
//        Set(esi, edx, Variable("z1"), Variable("z2")),
//        Set(edx, Variable("z2"), Variable("z1"), Variable("z3"))),
//      // (in <- edx)
//      //    (edx z1 z2 z3)       (in z1 z2 z3)
//      InstuctionInOutSet(Assignment(Variable("in"),edx),
//        Set(edx, Variable("z2"), Variable("z1"), Variable("z3")),
//        Set(Variable("z2"), Variable("z1"), Variable("in"), Variable("z3"))),
//      // (call :g)
//      //    (in z1 z2 z3)        (eax in z1 z2 z3)
//      InstuctionInOutSet(Call(Label("g")),
//        Set(Variable("z2"), Variable("z1"), Variable("in"), Variable("z3")),
//        Set(Variable("z2"), eax, Variable("z1"), Variable("in"), Variable("z3"))),
//      // (edx <- in)
//      //    (eax in z1 z2 z3)    (eax z1 z2 z3)
//      InstuctionInOutSet(Assignment(edx,Variable("in")),
//        Set(Variable("z2"), eax, Variable("z1"), Variable("in"), Variable("z3")),
//        Set(eax, Variable("z2"), Variable("z1"), Variable("z3"))),
//      // (g-ans <- eax)
//      //    (eax z1 z2 z3)       (g-ans z1 z2 z3)
//      InstuctionInOutSet(Assignment(Variable("g-ans"),eax),
//        Set(eax, Variable("z2"), Variable("z1"), Variable("z3")),
//        Set(Variable("z2"), Variable("z1"), Variable("z3"), Variable("g-ans"))),
//      // (call :h)
//      //    (g-ans z1 z2 z3)     (eax g-ans z1 z2 z3)
//      InstuctionInOutSet(Call(Label("h")),
//        Set(Variable("z2"), Variable("z1"), Variable("z3"), Variable("g-ans")),
//        Set(Variable("z2"), eax, Variable("z1"), Variable("z3"), Variable("g-ans"))),
//      // (eax += g-ans)
//      //    (eax g-ans z1 z2 z3) (z1 z2 z3)
//      InstuctionInOutSet(Increment(eax,Variable("g-ans")),
//        Set(Variable("z2"), eax, Variable("z1"), Variable("z3"), Variable("g-ans")),
//      // (ebx <- z1)
//      //    (z1 z2 z3)           (ebx z2 z3)
//        Set(Variable("z1"), Variable("z2"), Variable("z3"))),
//      InstuctionInOutSet(Assignment(ebx,Variable("z1")),
//        Set(Variable("z1"), Variable("z2"), Variable("z3")),
//        Set(Variable("z2"), Variable("z3"), ebx)),
//      // (edi <- z2)
//      //    (ebx z2 z3)          (ebx edi z3)
//      InstuctionInOutSet(Assignment(edi,Variable("z2")),
//        Set(Variable("z2"), Variable("z3"), ebx),
//        Set(Variable("z3"), ebx, edi)),
//      // (esi <- z3)
//      //    (ebx edi z3)         (ebx edi esi)
//      InstuctionInOutSet(Assignment(esi,Variable("z3")),
//        Set(Variable("z3"), ebx, edi),
//        Set(ebx, edi, esi)),
//      // (return)
//      //    (ebx edi esi)        ()
//      InstuctionInOutSet(Return,
//        Set(ebx, edi, esi),
//        Set())))
//  }
//
//  test("interference 1"){
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
//    assert(interferingVariables(code) ===
//            Set((Variable("3x"),Variable("2x2")), (Variable("2x2"),Variable("3x"))))
//  }
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
//  test("some live ranges"){
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
//    assert(liveRanges(inout(code)) === List(
//      List(LiveRange(Variable("z2"),9)),
//      List(LiveRange(eax,2), LiveRange(eax,1)),
//      List(LiveRange(Variable("in"),2)),
//      List(LiveRange(edi,2), LiveRange(edi,2)),
//      List(LiveRange(edx,5)),
//      List(LiveRange(esi,3), LiveRange(esi,1)),
//      List(LiveRange(Variable("g-ans"),2)),
//      List(LiveRange(Variable("z1"),9)),
//      List(LiveRange(Variable("z3"),9)),
//      List(LiveRange(ebx,1), LiveRange(ebx,3))))
//  }
//
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
