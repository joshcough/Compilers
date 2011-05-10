package L4Compiler

import util.TestHelpers
import L4Compiler.L4AST.E

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

  // this demonstrates the let renaming error from
  // http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture09.pdf (pg 25)
  // c is free outside the let, but becomes captured by it!
  compileETest("((let ((c a)) (b c)) c)", "(let ((c a)) (let ((__x0 (b c))) (__x0 c)))", changeVarNames = false)
  // to avoid this, we rename all the variables first
  compileETest("((let ((d a)) (b d)) d)", "(let ((__x0 a)) (let ((__x1 (b __x0))) (__x1 d)))",
    changeVarNames = true)

  def compileETest(code:String, expected:String, changeVarNames:Boolean = false) = {
    test(code.clean){
      val res = compileE(code.clean, changeVarNames = changeVarNames)
      //println("compileETest(\"" + code + "\", " + "\"" + L4Printer.toCode(res) + "\")")
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }

  def compileE(code:String, changeVarNames:Boolean = true, allowFrees:Boolean = false): E = {
    val compiler = new L4Compiler{}
    import compiler._
    val ast = parseE(read(code))
    if(changeVarNames) find(changeVarNamesInE(ast, allowFrees = true)) else find(ast)
  }
}

class ChangeVarNamesTests extends TestHelpers {

  // renames the let bound c only. the rest are free and this call allows frees.
  changeVarNamesInE("((let ((c a)) (b c)) c)", "((let ((__x0 a)) (b __x0)) c)", allowFrees = true)

  changeVarNamesInE("(let ((c 7)) (print c))", "(let ((__x0 7)) (print __x0))")

  changeVarNamesInE(
    "(let ((c 7)) (let ((c 7)) (print c)))",
    "(let ((__x0 7)) (let ((__x1 7)) (print __x1)))")

  changeVarNamesInE(
    "(let ((c 7)) (begin (let ((c 7)) (print c)) (print c)))",
    "(let ((__x0 7)) (begin (let ((__x1 7)) (print __x1)) (print __x0)))")

  changeVarNamesInE(
    "(let ((c 7)) (begin (let ((c c)) (print c)) (print c)))",
    "(let ((__x0 7)) (begin (let ((__x1 __x0)) (print __x1)) (print __x0)))")

  changeVarNamesInFunc(
    "(:f (x) (let ((c x)) (begin (let ((c x)) (print c)) (print c))))",
    "(:f (__x0) (let ((__x1 __x0)) (begin (let ((__x2 __x0)) (print __x2)) (print __x1))))")

  changeVarNamesInFunc(
    "(:f (x y z) (x y z))",
    "(:f (__x0 __x1 __x2) (__x0 __x1 __x2))")

  def changeVarNamesInE(code:String, expected:String, allowFrees:Boolean=false) = {
    test(code.clean){
      val compiler = new L4Compiler{}
      val res = compiler.changeVarNamesInE(compiler.parseE(compiler.read(code.clean)), allowFrees=allowFrees)
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }

  def changeVarNamesInFunc(code:String, expected:String) = {
    test(code.clean){
      val compiler = new L4Compiler{}
      val res = compiler.changeVarNamesInFunc(compiler.parseFunction(compiler.read(code.clean)))
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }
}
