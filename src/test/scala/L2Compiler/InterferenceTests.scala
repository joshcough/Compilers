package L2Compiler

import L2AST._

class InterferenceTests extends L2CompilerTest {

//
//  test("interference 1"){
//    val code = """
//      |(:f
//      |(x2 <- eax)
//      |(x2 *= x2)
//      |(2x2 <- x2)
//      |(2x2 *= 2)
//      |(3x <- eax)
//      |(3x *= 3)
//      |(eax <- 2x2)
//      |(eax += 3x)
//      |(eax += 4)
//      |(return))""".stripMargin.trim
//
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
