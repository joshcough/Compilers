package L2Compiler

import org.scalatest.FunSuite
import L2AST._

class InterferenceGraphTests extends FunSuite {

  val x = Variable("x")

  test("hwView") {
    assert(InterferenceGraph.base.hwView === """
((eax ebx ecx edi edx esi)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi)
(edx eax ebx ecx edi esi)
(esi eax ebx ecx edi edx))""".trim)
  }

  test("hw view with some added interference") {
    assert(InterferenceGraph.base.addInterference(eax -> x, edi -> x, esi -> x).hwView === """
((eax ebx ecx edi edx esi x)
(ebx eax ecx edi edx esi)
(ecx eax ebx edi edx esi)
(edi eax ebx ecx edx esi x)
(edx eax ebx ecx edi esi)
(esi eax ebx ecx edi edx x)
(x eax edi esi))""".trim)
  }
}