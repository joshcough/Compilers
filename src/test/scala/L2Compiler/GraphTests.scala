package L2Compiler

import org.scalatest.FunSuite
import Graph._
import L2AST._

class GraphTests extends FunSuite {
  test("create graph"){
    assert(Graph("a" <-> "b") === Graph(Set("a" <-> "b", "b" <-> "a")))
    assert(Graph("a" <-> "b", "b" <-> "a") === Graph(Set("a" <-> "b", "b" <-> "a")))
    assert(Graph("a" <-> "b", "a" <-> "c", "b" <-> "c") ===
            Graph(Set(
              "a" <-> "b", "b" <-> "a",
              "a" <-> "c", "c" <-> "a",
              "b" <-> "c", "c" <-> "b")))
  }
  test("dups ignored"){
    assert(Graph("a" <-> "b", "b" <-> "a", "a"<->"b") === Graph(Set("a" <-> "b", "b" <-> "a")))
    assert(Graph("a" <-> "b", "b" <-> "a", "b"<->"a") === Graph(Set("a" <-> "b", "b" <-> "a")))
  }

  test("register graph"){
    assert(Graph(eax <-> ecx) === Graph(Set(eax <-> ecx, ecx <-> eax)))
    val g = Graph(eax <-> Variable("x"))
    println(g)
    assert(g === Graph(Set(eax <-> Variable("x"), Variable("x") <-> eax)))
  }
}