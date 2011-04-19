package L2Compiler

import org.scalatest.FunSuite
import L2AST._

class BiDirectionalGraphTests extends FunSuite {
  test("create graph"){
    val g = BiDirectionalGraph("a" -> "b")
    assert(g.contains("a" -> "b"))
    assert(g.contains("b" -> "a"))
  }
  test("create graph 2"){
    val g = BiDirectionalGraph("a" -> "b", "b" -> "a")
    assert(g.contains("a" -> "b"))
    assert(g.contains("b" -> "a"))
  }
  test("create graph 3"){
    val g = BiDirectionalGraph("a" -> "b", "a" -> "c", "b" -> "c")
    assert(g.contains("a" -> "b"))
    assert(g.contains("b" -> "a"))
    assert(g.contains("a" -> "c"))
    assert(g.contains("c" -> "a"))
    assert(g.contains("b" -> "c"))
    assert(g.contains("c" -> "b"))
  }
  test("dups ignored"){
    val g = BiDirectionalGraph("a" -> "b", "b" -> "a", "a"->"b")
    assert(g.contains("a" -> "b"))
    assert(g.contains("b" -> "a"))
    val g2 = BiDirectionalGraph("a" -> "b", "b" -> "a", "b"->"a")
    assert(g2.contains("a" -> "b"))
    assert(g2.contains("b" -> "a"))
  }

  test("register graph"){
    val g = BiDirectionalGraph(eax -> ecx)
    assert(g.contains(eax -> ecx))
    assert(g.contains(ecx -> eax))
  }

  test("replace"){
    val g = BiDirectionalGraph(eax -> ecx).replace(eax, ebx)
    assert(g.contains(ebx -> ecx))
    assert(g.contains(ecx -> ebx))
  }

  test("register graph 2"){
    val x = Variable("x")
    val g = BiDirectionalGraph(eax -> x)
    assert(g.contains(eax -> x))
    assert(g.contains(x -> eax))
  }

//
//  test("simple color graph"){
//    val g = BiDirectionalGraph(new ColoredNode(eax, GRAY) -> (ebx -> GRAY))
//    assert(g.contains(new ColoredNode(eax, GRAY) -> (ebx -> GRAY)))
//    assert(g.contains(new ColoredNode(ebx, GRAY) -> new ColoredNode(eax, GRAY)))
//  }
//
//  test("replace in simple color graph"){
//    val g = BiDirectionalGraph[(X, Color)](new ColoredNode(eax, GRAY) -> (ebx -> GRAY)).replace((eax,GRAY), (eax, GREEN))
//    assert(g.contains((eax, GREEN) -> (ebx -> GRAY)))
//    assert(g.contains(new ColoredNode(ebx, GRAY) -> (eax -> GREEN)))
//  }
//
//  test("color simple color graph"){
//    val g = RegisterColorGraph(
//      BiDirectionalGraph(new ColoredNode(eax, GRAY) -> new ColoredNode(ebx, GRAY))).color.get
//    assert(g.data.contains(new ColoredNode(eax, RED) -> new ColoredNode(ebx, GREEN)))
//    assert(g.data.contains(new ColoredNode(ebx, GREEN) -> new ColoredNode(eax, RED)))
//  }
//
//  test("color simple color graph 2"){
//    // adding one more node, but the node is only connected to eax, and
//    // so it can also get green
//    val g = RegisterColorGraph(
//      BiDirectionalGraph(
//        new ColoredNode(eax, GRAY) -> new ColoredNode(ebx, GRAY),
//        new ColoredNode(eax, GRAY) -> new ColoredNode(ecx, GRAY))).color.get
//    assert(g.data.contains(new ColoredNode(eax, RED)   -> new ColoredNode(ebx, GREEN)))
//    assert(g.data.contains(new ColoredNode(ebx, GREEN) -> new ColoredNode(eax, RED)))
//    assert(g.data.contains(new ColoredNode(eax, RED)   -> new ColoredNode(ecx, GREEN)))
//    assert(g.data.contains(new ColoredNode(ecx, GREEN) -> new ColoredNode(eax, RED)))
//
//    // but as soon as you attach ebx to ecx, the graph must have 3 colors
//    val g2 = RegisterColorGraph(BiDirectionalGraph(
//      new ColoredNode(eax, GRAY) -> new ColoredNode(ebx, GRAY),
//      new ColoredNode(eax, GRAY) -> new ColoredNode(ecx, GRAY),
//      new ColoredNode(ebx, GRAY) -> new ColoredNode(ecx, GRAY))).color.get
//
//    assert(g2.data.contains(new ColoredNode(eax, BLUE)  -> new ColoredNode(ebx, RED)))
//    assert(g2.data.contains(new ColoredNode(eax, BLUE)  -> new ColoredNode(ecx, GREEN)))
//    assert(g2.data.contains(new ColoredNode(ecx, GREEN) -> new ColoredNode(eax, BLUE)))
//    assert(g2.data.contains(new ColoredNode(ebx, RED)   -> new ColoredNode(ecx, GREEN)))
//    assert(g2.data.contains(new ColoredNode(ecx, GREEN) -> new ColoredNode(ebx, RED)))
//  }
//
//  test("base register color graph"){
//    val g = RegisterColorGraph.base
//    assert(g.data.contains(new ColoredNode(eax, RED) -> new ColoredNode(esi, MAGENTA)))
//    assert(g.data.contains(new ColoredNode(esi, MAGENTA) -> new ColoredNode(eax, RED)))
//  }
}