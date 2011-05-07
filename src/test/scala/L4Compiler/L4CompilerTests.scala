package L4Compiler

import util.TestHelpers

class L4CompilerTests extends TestHelpers{

  compileETest("7", "7")
  compileETest("(b c)", "(b c)")
  compileETest("(a (b c))", "(let ((__x0 (b c))) (a __x0))")
  compileETest(
    "(let (a (b (c d))) 7)",
    "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (let ((__x2 (a __x1))) (let __x2))))")
  compileETest("(print (a b))", "(let ((__x0 (a b))) (print __x0))")
  compileETest("(+ a b)", "(+ a b)")
  compileETest("(+ (a b) c)", "(let ((__x0 (a b))) (+ __x0 c))")
  compileETest("(+ c (a b))", "(let ((__x0 (a b))) (+ c __x0))")
  compileETest("(+ (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (+ __x0 __x1)))")
  compileETest("(- a b)", "(- a b)")
  compileETest("(- (a b) c)", "(let ((__x0 (a b))) (- __x0 c))")
  compileETest("(- c (a b))", "(let ((__x0 (a b))) (- c __x0))")
  compileETest("(- (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (- __x0 __x1)))")
  compileETest("(* a b)", "(* a b)")
  compileETest("(* (a b) c)", "(let ((__x0 (a b))) (* __x0 c))")
  compileETest("(* c (a b))", "(let ((__x0 (a b))) (* c __x0))")
  compileETest("(* (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (* __x0 __x1)))")
  compileETest("(< a b)", "(< a b)")
  compileETest("(< (a b) c)", "(let ((__x0 (a b))) (< __x0 c))")
  compileETest("(< c (a b))", "(let ((__x0 (a b))) (< c __x0))")
  compileETest("(< (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (< __x0 __x1)))")
  compileETest("(<= a b)", "(<= a b)")
  compileETest("(<= (a b) c)", "(let ((__x0 (a b))) (<= __x0 c))")
  compileETest("(<= c (a b))", "(let ((__x0 (a b))) (<= c __x0))")
  compileETest("(<= (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (<= __x0 __x1)))")
  compileETest("(= a b)", "(= a b)")
  compileETest("(= (a b) c)", "(let ((__x0 (a b))) (= __x0 c))")
  compileETest("(= c (a b))", "(let ((__x0 (a b))) (= c __x0))")
  compileETest("(= (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (= __x0 __x1)))")

  compileETest("((b c) (+ (- c d) (* 1 2)))",
    "(let ((__x0 (b c))) (let ((__x1 (- c d))) (let ((__x2 (* 1 2))) (let ((__x3 (+ __x1 __x2))) (__x0 __x3)))))")



  def compileETest(code:String, expected:String) = {
    test(code){
      verboseAssert(code, new L4Compiler{}.compileE(code), expected)
    }
  }
}
