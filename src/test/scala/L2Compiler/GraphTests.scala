package L2Compiler

import org.scalatest.FunSuite
import Graph._

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
}