package L2Compiler

import RegisterColorGraph._
import L1Compiler.L1AST._
import L2AST._

class LivenessTest extends L2CompilerTest {

  import compiler._

  test("from homework"){
    // should result in:
    // ((in (eax) (eax x)) (out (eax x) ()))
    val code = "((x <- 1) (eax += x)) x -4 s"
    assert(inout(code) === List(
      InstuctionInOutSet(
        Assignment(Variable("x"),Num(1)),
          Set(eax),
          Set(eax, Variable("x"))),
      InstuctionInOutSet(
        Increment(eax,Variable("x")),
          Set(eax, Variable("x")),
          Set())))
  }

  test("test from lecture notes") {
    val code = """
 ((x2 <- edx)
 (x2 *= x2)
 (2x2 <- x2)
 (2x2 *= 2)
 (3x <- edx)
 (3x *= 3)
 (eax <- 2x2)
 (eax += 3x)
 (eax += 4)
 (return))
"""
    assert(inout(code) ===
    List(
//      // :main
//      //  (edx)   (ebx edi edx esi)
//      InstuctionInOutSet(
//        LabelDeclaration(Label("main")),
//          Set(edx),
//          Set(edx, edi, esi, ebx)),
      // (x2 <- edx)
      // (ebx edi edx esi)     (ebx edi edx esi x2)
      InstuctionInOutSet(
        Assignment(Variable("x2"),edx),
          Set(edx, edi, esi, ebx),
          Set(Variable("x2"), edi, edx, esi, ebx)),
      // (x2 *= x2)
      // (ebx edi edx esi x2)  (ebx edi edx esi x2)
      InstuctionInOutSet(
        Multiply(Variable("x2"),Variable("x2")),
          Set(Variable("x2"), edi, edx, esi, ebx),
          Set(Variable("x2"), edi, edx, esi, ebx)),
      // (2x2 <- x2)
      // (ebx edi edx esi x2)  (2x2 ebx edi edx esi)
      InstuctionInOutSet(Assignment(Variable("2x2"),Variable("x2")),
        Set(Variable("x2"), edi, edx, esi, ebx),
        Set(Variable("2x2"), edi, edx, esi, ebx)),
      //  (2x2 *= 2)
      //  (2x2 ebx edi edx esi) (2x2 ebx edi edx esi)
      InstuctionInOutSet(Multiply(Variable("2x2"),Num(2)),
        Set(Variable("2x2"), edi, edx, esi, ebx),
        Set(Variable("2x2"), edi, edx, esi, ebx)),
      //  (3x <- edx)
      //  (2x2 ebx edi edx esi) (2x2 3x ebx edi esi)
      InstuctionInOutSet(Assignment(Variable("3x"),edx),
        Set(Variable("2x2"), edi, edx, esi, ebx),
        Set(Variable("2x2"), edi, Variable("3x"), esi, ebx)),
      //  (3x *= 3)
      //  (2x2 3x ebx edi esi)  (2x2 3x ebx edi esi)
      InstuctionInOutSet(Multiply(Variable("3x"),Num(3)),
        Set(Variable("2x2"), edi, Variable("3x"), esi, ebx),
        Set(Variable("2x2"), edi, Variable("3x"), esi, ebx)),
      //  (eax <- 2x2)
      //  (2x2 3x ebx edi esi)  (3x eax ebx edi esi)
      InstuctionInOutSet(Assignment(eax,Variable("2x2")),
        Set(Variable("2x2"), edi, Variable("3x"), esi, ebx),
        Set(eax, edi, Variable("3x"), esi, ebx)),
      //  (eax += 3x)
      //  (3x eax ebx edi esi)  (eax ebx edi esi)
      InstuctionInOutSet(Increment(eax,Variable("3x")),
        Set(eax, edi, Variable("3x"), esi, ebx),
        Set(eax, ebx, edi, esi)),
      //  (eax += 4)
      //  (eax ebx edi esi)     (ebx edi esi)
      InstuctionInOutSet(Increment(eax,Num(4)),
        Set(eax, ebx, edi, esi),
        Set(ebx, edi, esi)),
      //  (return)
      // (ebx edi esi)         ()
      InstuctionInOutSet(Return,
        Set(ebx, edi, esi),Set())))

  }

  test("call"){
 val code = """
((in <- edx)
 (call :g)
 (edx <- in)
 (g-ans <- eax)
 (call :h)
 (eax += g-ans)
 (return))"""
    assert(inout(code) === List(
//      // :f
//      // (edx)                   (ebx edi edx esi)
//      InstuctionInOutSet(LabelDeclaration(Label("main")),
//        Set(edx),
//        Set(edx, edi, esi, ebx)),
      // (in <- edx)
      // (ebx edi edx esi)       (ebx edi esi in)
      InstuctionInOutSet(Assignment(Variable("in"),edx),
        Set(edx, edi, esi, ebx),
        Set(Variable("in"), edi, esi, ebx)),
      // (call :g)
      // (ebx edi esi in)        (eax ebx edi esi in)
      InstuctionInOutSet(Call(Label("g")),
        Set(Variable("in"), edi, esi, ebx),
        Set(eax, Variable("in"), edi, esi, ebx)),
      //  (edx <- in)
      //  (eax ebx edi esi in)    (eax ebx edi esi)
      InstuctionInOutSet(Assignment(edx,Variable("in")),
        Set(eax, Variable("in"), edi, esi, ebx),
        Set(eax, ebx, edi, esi)),
      // (g-ans <- eax)
      // (eax ebx edi esi)       (ebx edi esi g-ans)
      InstuctionInOutSet(Assignment(Variable("g-ans"),eax),
        Set(eax, ebx, edi, esi),
        Set(ebx, edi, esi, Variable("g-ans"))),
      // (call :h)
      // (ebx edi esi g-ans)     (eax ebx edi esi g-ans)
      InstuctionInOutSet(Call(Label("h")),
        Set(ebx, edi, esi, Variable("g-ans")),
        Set(ebx, eax, edi, esi, Variable("g-ans"))),
      // (eax += g-ans)
      // (eax ebx edi esi g-ans) (ebx edi esi)
      InstuctionInOutSet(Increment(eax,Variable("g-ans")),
        Set(ebx, eax, edi, esi, Variable("g-ans")),
        Set(ebx, edi, esi)),
      // (return)
      // (ebx edi esi)           ()
      InstuctionInOutSet(Return,
        Set(ebx, edi, esi),
        Set())))
  }

  test("more"){
    val code = """
 ((z1 <- ebx)
 (z2 <- edi)
 (z3 <- esi)
 (in <- edx)
 (call :g)
 (edx <- in)
 (g-ans <- eax)
 (call :h)
 (eax += g-ans)
 (ebx <- z1)
 (edi <- z2)
 (esi <- z3)
 (return))
"""
    assert(inout(code) === List(
//      // :main
//      //    (edx)                (ebx edi edx esi)
//      InstuctionInOutSet(LabelDeclaration(Label("main")),
//        Set(edx),
//        Set(ebx, edi, esi, edx)),
      // (z1 <- ebx)
      //    (ebx edi edx esi)    (edi edx esi z1)
      InstuctionInOutSet(Assignment(Variable("z1"),ebx),
        Set(ebx, edi, esi, edx),
        Set(edi, esi, edx, Variable("z1"))),
      // (z2 <- edi)
      //    (edi edx esi z1)     (edx esi z1 z2)
      InstuctionInOutSet(Assignment(Variable("z2"),edi),
        Set(edi, esi, edx, Variable("z1")),
        Set(esi, edx, Variable("z1"), Variable("z2"))),
      // (z3 <- esi)
      //    (edx esi z1 z2)      (edx z1 z2 z3)
      InstuctionInOutSet(Assignment(Variable("z3"),esi),
        Set(esi, edx, Variable("z1"), Variable("z2")),
        Set(edx, Variable("z2"), Variable("z1"), Variable("z3"))),
      // (in <- edx)
      //    (edx z1 z2 z3)       (in z1 z2 z3)
      InstuctionInOutSet(Assignment(Variable("in"),edx),
        Set(edx, Variable("z2"), Variable("z1"), Variable("z3")),
        Set(Variable("z2"), Variable("z1"), Variable("in"), Variable("z3"))),
      // (call :g)
      //    (in z1 z2 z3)        (eax in z1 z2 z3)
      InstuctionInOutSet(Call(Label("g")),
        Set(Variable("z2"), Variable("z1"), Variable("in"), Variable("z3")),
        Set(Variable("z2"), eax, Variable("z1"), Variable("in"), Variable("z3"))),
      // (edx <- in)
      //    (eax in z1 z2 z3)    (eax z1 z2 z3)
      InstuctionInOutSet(Assignment(edx,Variable("in")),
        Set(Variable("z2"), eax, Variable("z1"), Variable("in"), Variable("z3")),
        Set(eax, Variable("z2"), Variable("z1"), Variable("z3"))),
      // (g-ans <- eax)
      //    (eax z1 z2 z3)       (g-ans z1 z2 z3)
      InstuctionInOutSet(Assignment(Variable("g-ans"),eax),
        Set(eax, Variable("z2"), Variable("z1"), Variable("z3")),
        Set(Variable("z2"), Variable("z1"), Variable("z3"), Variable("g-ans"))),
      // (call :h)
      //    (g-ans z1 z2 z3)     (eax g-ans z1 z2 z3)
      InstuctionInOutSet(Call(Label("h")),
        Set(Variable("z2"), Variable("z1"), Variable("z3"), Variable("g-ans")),
        Set(Variable("z2"), eax, Variable("z1"), Variable("z3"), Variable("g-ans"))),
      // (eax += g-ans)
      //    (eax g-ans z1 z2 z3) (z1 z2 z3)
      InstuctionInOutSet(Increment(eax,Variable("g-ans")),
        Set(Variable("z2"), eax, Variable("z1"), Variable("z3"), Variable("g-ans")),
      // (ebx <- z1)
      //    (z1 z2 z3)           (ebx z2 z3)
        Set(Variable("z1"), Variable("z2"), Variable("z3"))),
      InstuctionInOutSet(Assignment(ebx,Variable("z1")),
        Set(Variable("z1"), Variable("z2"), Variable("z3")),
        Set(Variable("z2"), Variable("z3"), ebx)),
      // (edi <- z2)
      //    (ebx z2 z3)          (ebx edi z3)
      InstuctionInOutSet(Assignment(edi,Variable("z2")),
        Set(Variable("z2"), Variable("z3"), ebx),
        Set(Variable("z3"), ebx, edi)),
      // (esi <- z3)
      //    (ebx edi z3)         (ebx edi esi)
      InstuctionInOutSet(Assignment(esi,Variable("z3")),
        Set(Variable("z3"), ebx, edi),
        Set(ebx, edi, esi)),
      // (return)
      //    (ebx edi esi)        ()
      InstuctionInOutSet(Return,
        Set(ebx, edi, esi),
        Set())))
  }

  test("interference 1"){
    val code = """
 ((x2 <- edx)
 (x2 *= x2)
 (2x2 <- x2)
 (2x2 *= 2)
 (3x <- edx)
 (3x *= 3)
 (eax <- 2x2)
 (eax += 3x)
 (eax += 4)
 (return))
"""
    assert(interferingVariables(code) ===
            Set((Variable("3x"),Variable("2x2")), (Variable("2x2"),Variable("3x"))))
  }

  test("interference 2"){
        val code = """
 ((z1 <- ebx)
 (z2 <- edi)
 (z3 <- esi)
 (in <- edx)
 (call :g)
 (edx <- in)
 (g-ans <- eax)
 (call :h)
 (eax += g-ans)
 (ebx <- z1)
 (edi <- z2)
 (esi <- z3)
 (return))
"""
    val interference = interferingVariables(code)
    val expected = Set((Variable("z1"),Variable("z2")), (Variable("z2"),Variable("z1")),
        (Variable("g-ans"),Variable("z2")), (Variable("z2"),Variable("g-ans")),
        (Variable("z2"),Variable("in")), (Variable("in"),Variable("z2")),
        (Variable("g-ans"),Variable("z1")), (Variable("z1"),Variable("g-ans")),
        (Variable("z3"),Variable("z1")), (Variable("z1"),Variable("z3")),
        (Variable("in"),Variable("z1")), (Variable("z1"),Variable("in")),
        (Variable("z3"),Variable("z2")), (Variable("z2"),Variable("z3")),
        (Variable("z3"),Variable("g-ans")), (Variable("g-ans"),Variable("z3")),
        (Variable("z3"),Variable("in")), (Variable("in"),Variable("z3"))
        )
    assert(interference === expected)
  }

  test("legit graph coloring"){
    val code = """
 ((x2 <- edx)
 (x2 *= x2)
 (2x2 <- x2)
 (2x2 *= 2)
 (3x <- edx)
 (3x *= 3)
 (eax <- 2x2)
 (eax += 3x)
 (eax += 4)
 (return))
"""
    // this must be colorable.
    val coloredGraph = attemptToColor(code).get
    assert(coloredGraph.colorOf(Variable("x2")) === RED)
    assert(coloredGraph.colorOf(Variable("2x2")) === RED)
    assert(coloredGraph.colorOf(Variable("3x")) === GREEN)
  }

  test("uncolorable"){
    val code = """
            (((mem ebp -4) <- edx)
            (call :g)
            (edx <- (mem ebp -4))
            (g-ans <- eax)
            (call :h)
            (eax += g-ans)
            (return))"""
    println(inout(code))
    //println(attemptToColor(code))
  }


  test("some live ranges"){
    val code = """
 ((z1 <- ebx)
 (z2 <- edi)
 (z3 <- esi)
 (in <- edx)
 (call :g)
 (edx <- in)
 (g-ans <- eax)
 (call :h)
 (eax += g-ans)
 (ebx <- z1)
 (edi <- z2)
 (esi <- z3)
 (return))
"""
    assert(liveRanges(inout(code)) === List(
      List(LiveRange(Variable("z2"),9)),
      List(LiveRange(eax,2), LiveRange(eax,1)),
      List(LiveRange(Variable("in"),2)),
      List(LiveRange(edi,2), LiveRange(edi,2)),
      List(LiveRange(edx,4)),
      List(LiveRange(esi,3), LiveRange(esi,1)),
      List(LiveRange(Variable("g-ans"),2)),
      List(LiveRange(Variable("z1"),9)),
      List(LiveRange(Variable("z3"),9)),
      List(LiveRange(ebx,1), LiveRange(ebx,3))))

  }

  test("choose spill variable"){
    val code = """
 ((z1 <- ebx)
 (z2 <- edi)
 (z3 <- esi)
 (in <- edx)
 (call :g)
 (edx <- in)
 (g-ans <- eax)
 (call :h)
 (eax += g-ans)
 (ebx <- z1)
 (edi <- z2)
 (esi <- z3)
 (return))
"""
    assert(chooseSpillVar(liveRanges(inout(code))) === Some(Variable("z2")))
  }
}