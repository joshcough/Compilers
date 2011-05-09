package L4Compiler

import util.TestHelpers

class L4CompilerTests extends TestHelpers{

  compileETest("7", "7")
  compileETest("(b c)", "(b c)")
  compileETest("(a b c)", "(a b c)")
  compileETest("(a (b c))", "(let ((__x0 (b c))) (a __x0))")
  compileETest("(a (b (c d)))", "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (a __x1)))")
  compileETest("(a (b (c d)) (x (y z)))",
    "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (let ((__x2 (y z))) (let ((__x3 (x __x2))) (a __x1 __x3)))))")
  compileETest("(a (b (c d)) 7 (x (y z)))",
    "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (let ((__x2 (y z))) (let ((__x3 (x __x2))) (a __x1 7 __x3)))))")
  compileETest("(let ((a 8)) 7)", "(let ((a 8)) 7)")
  compileETest("(let ((a (b c))) 7)", "(let ((a (b c))) 7)")
  compileETest("(let ((a (b (c d)))) 7)", "(let ((__x0 (c d))) (let ((a (b __x0))) 7))")
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
  compileETest("(number? 7)", "(number? 7)")
  compileETest("(number? c)", "(number? c)")
  compileETest("(number? (b c))", "(let ((__x0 (b c))) (number? __x0))")
  compileETest("(a? 7)", "(a? 7)")
  compileETest("(a? c)", "(a? c)")
  compileETest("(a? (b c))", "(let ((__x0 (b c))) (a? __x0))")

  compileETest("(new-tuple a b c d e f g h i j k)", "(new-tuple a b c d e f g h i j k)")

  compileETest("(new-tuple a b c (+ d d) e f g h i j k)", "(let ((__x0 (+ d d))) (new-tuple a b c __x0 e f g h i j k))")
  compileETest("(new-tuple a b c d e f g h i j (+ k k))", "(let ((__x0 (+ k k))) (new-tuple a b c d e f g h i j __x0))")

  compileETest("(begin x y)", "(let ((__x0 x)) y)")
  compileETest("(begin (+ 1 2) y)", "(let ((__x0 (+ 1 2))) y)")
  compileETest("(begin y (+ 1 2))", "(let ((__x0 y)) (+ 1 2))")
  compileETest("(begin (+ 1 2) (+ 1 2))", "(let ((__x0 (+ 1 2))) (+ 1 2))")
  compileETest("(begin (a (b c)) 7)", "(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))")

  // these are just to show that what the begin code translates to is good.
  compileETest("(let ((__x0 (+ 1 2))) y)", "(let ((__x0 (+ 1 2))) y)")
  compileETest("(let ((__x0 y)) (+ 1 2))", "(let ((__x0 y)) (+ 1 2))")
  compileETest("(let ((__x0 (+ 1 2))) (+ 1 2))", "(let ((__x0 (+ 1 2))) (+ 1 2))")
  compileETest("(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))", "(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))")

  //(let ((passed (aref 0 results))) (new-tuple (if pass? (+ 1 passed) passed) (+ 1 (aref 1 results))))
  compileETest("""
(new-tuple
(let ((passed (aref 0 results)))
(if pass? (+ 1 passed) passed))
(+ 1 (aref 1 results)))""",
"""
(let ((passed (aref 0 results)))
(if pass?
(let ((__x0 (+ 1 passed)))
(let ((__x1 (aref 1 results)))
(let ((__x2 (+ 1 __x1)))
(new-tuple __x0 __x2))))
(let ((__x3 (aref 1 results)))
(let ((__x4 (+ 1 __x3)))
(new-tuple passed __x4)))))""")


  compileETest("((let ((c a)) (b c)) c))", "?")

  def compileETest(code:String, expected:String) = {
    test(code.clean){
      val compiler = new L4Compiler{}
      val res = compiler.compileE(code.clean)
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }
}
