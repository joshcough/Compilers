package L2Compiler

import L2AST._

class InterferenceGraphTests extends L2CompilerTest {

  val x = Variable("x")

  test("base graph with just the registers") {
    assert(InterferenceGraph.base.hwView === """
((eax ebx ecx edi edx esi)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi)
(edx eax ebx ecx edi esi)
(esi eax ebx ecx edi edx))""".trim)
  }

  test("base graph with some added interference") {
    assert(InterferenceGraph.base.addInterference(eax -> x, edi -> x, esi -> x).hwView === """
((eax ebx ecx edi edx esi x)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi x)
(edx eax ebx ecx edi esi)
(esi eax ebx ecx edi edx x)
(x eax edi esi))""".trim)
  }

  test("http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture05.pdf (p 111)"){
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
    val insAndOuts = compiler.inoutForTesting(code.clean, step=End)
    val actual = InterferenceGraph.buildInterferenceSet(insAndOuts).hwView
    assert( actual === """
((2x2 3x eax edi esi)
(3x 2x2 eax edi esi)
(eax 2x2 3x ebx ecx edi edx esi x2)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi 2x2 3x eax ebx ecx edx esi x2)
(edx eax ebx ecx edi esi)
(esi 2x2 3x eax ebx ecx edi edx x2)
(x2 eax edi esi))""".trim)
  }

}