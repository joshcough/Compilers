package L4Compiler

import util.{SlowTest, L3Interpreter, L4Interpreter, TestHelpers}
import L3Compiler.L3Printer

class L4CompilerTests extends TestHelpers with SlowTest {

  testCompile("(:test 5 5)")
  testCompile("(:test (:eq (new-tuple) (new-tuple)) 1)")
  testCompile("(:test (:eq 1 2) 0)")
  testCompile("(:test (:nil) (new-tuple))")
  testCompile("(:test (:nil) (:nil))")
  testCompile("(:test (:none) (:none))")
  testCompile("(:test (:some 7) (:some 7))")
  testCompile("(:test (:option_get (:some 7)) 7)")
  testCompile("(:test (:tolist (new-tuple (+ 5 6) (+ 6 7) 7)) (:tolist (new-tuple 11 13 7)))")
  testCompile("(:test (< 3 4) 1)")
  testCompile("(:test (< 4 3) 0)")
  testCompile("(:test (if (< 3 4) 5 6) 5)")
  testCompile("(:test (if (< 4 3) 5 6) 6)")
  testCompile("(:test (let ([x (let ([x 5]) x)]) x) 5)")
  testCompile("(:test (let ([x 4]) (let ([y x]) (let ([x 3]) (+ (:sq y) (:sq x))))) 25)")
  testCompile("""
    (:test
      (let ([x (make-closure :sq 7)])
        (let ([y (make-closure :double 7)])
          (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y)))
        )
      )
      63
    )
    """)
  testCompile("""
    (:test
      (let ([x (make-closure :sq 7)])
        (let ([y (make-closure :double 7)])
          (:sqdouble (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y))))
        )
      )
    15876)
  """)
  testCompile("(:test (:f 5) 5)", "(:f (x) (:identity x))")
  testCompile("(:test (:cons 1 (:cons 2 (:cons 3 (:nil)))) (:tolist (new-tuple 1 2 3)))")
  testCompile("(:test (:cons 1 (:cons 2 (:cons 3 (:cons 4 (:nil))))) (:tolist (new-tuple 1 2 3 4)))")
  testCompile("""
    (:test
      (:cons 1 (:cons 2 (:cons 3 (:cons 4 (:cons 5 (:cons 6 (:cons 7 (:cons 8 (:nil)))))))))
      (:tolist (new-tuple 1 2 3 4 5 6 7 8)))""")

  testCompile("(:test (:map (:nil) :sq) (:nil))")
  testCompile("(:test (:map (:tolist (new-tuple 1 2 3 4)) :sq) (:tolist (new-tuple 1 4 9 16)))")
  testCompile("(:test (:filter (:nil) :greaterthan3) (:nil))")
  testCompile("(:test (:filter (:tolist (new-tuple 1 2 3 4 5 6 7)) :greaterthan3) (:tolist (new-tuple 4 5 6 7)))")
  testCompile("(:test (:and 1 1) 1)")
  testCompile("(:test (:and 1 0) 0)")
  testCompile("(:test (:and 0 1) 0)")
  testCompile("(:test (:and 0 0) 0)")
  testCompile("(:test (:or 1 1) 1)")
  testCompile("(:test (:or 1 0) 1)")
  testCompile("(:test (:or 0 1) 1)")
  testCompile("(:test (:or 0 0) 0)")
  testCompile("(:test (:foldl :add 0 (:tolist (new-tuple 1 2 3 4 5 6))) 21)")
  testCompile("(:test (:foldl :mult 1 (:tolist (new-tuple 1 2 3 4 5 6))) 720)")
  testCompile("(:test (:find (:tolist (new-tuple 1 2 3 4 5 6 7)) :greaterthan3) (:some 4))")
  testCompile("(:test (:find (:tolist (new-tuple 1 2 2 2 2 2 2)) :greaterthan3) (:none))")
  testCompile("""
    (:test
      (:zip (:tolist (new-tuple 1 2 3)) (:tolist (new-tuple 1 2 3)))
      (:tolist (new-tuple (:tolist (new-tuple 1 1)) (:tolist (new-tuple 2 2)) (:tolist (new-tuple 3 3))))
    )""")
  testCompile("(:test (:eq_arr (new-tuple 1 2 3 4) (new-tuple 1 2 3 4)) 1)")
  testCompile("(:test (:eq_arr (new-array 5 5) (new-array 5 5)) 1)")
  testCompile("(:test (:eq_arr (new-array 5 5) (new-tuple 5 5 5 5 5)) 1)")
  testCompile("(:test (:foreach (:tolist (new-tuple 1 2 3 4 5 6 7)) :sq) 1)")
  testCompile("(:test (+ (:f (+ 5 5)) (:f (+ 5 5))) 80)", "(:f (x) (+ (+ x x) (+ x x)))")
  testCompile("(:test (- (:f (+ 7 5)) (:f (* 2 (+ 1 0)))) 20)", "(:f (x) (+ (+ x x) (= x x)))")
  testCompile("(:test (let ([c 5]) (let ([a 10]) (let ([c a]) (let ([b :b]) (b c))))) 6)", "(:b (x) 6)")
  testCompile("""
    (:test
      (begin
        (let ([x (make-closure :sq 7)])
          (let ([y (make-closure :double 7)])
            (:sqdouble (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y))))
          )
        )
        (+ (+ 2 3) (+ 3 3))
      )
      11
    )
  """)
  testCompile("""
    (:test
      (begin
        (begin 6 7)
        (begin 8 9))
      9
    )""")
  testCompile("""
    (:test
      (begin
        (begin (begin
        (begin (begin
        (begin (begin
        (begin (:and 1 1) 7)
        (begin (begin
        (begin 6 7)
        (begin 8 9)) 9)) (:map (:nil) :sq))
        (begin 8 9)) (begin
        (begin (:foldl :add 0 (:tolist (new-tuple 1 2 3 4 5 6))) 7)
        (begin (:sq 7) 9)))
        (begin (begin
        (begin 6 7)
        (begin (begin
        (begin (:cons 1 (:cons 2 (:cons 3 (:nil)))) 7)
        (begin (if (< 4 3) 5 6) 9)) 9)) (begin
        (begin 6 7)
        (begin 8 9)))) (begin
        (begin (:eq_arr (new-array 5 5) (new-tuple 5 5 5 5 5)) 7)
        (begin (begin
        (begin (:nil) (< 7 7))
        (begin (= 5 6) (<= 7 8))) 9)))
        (begin (begin
        (begin (= 5 (= 5 (= 5 6))) 7)
        (begin (new-tuple (new-tuple (new-tuple)) (new-tuple (new-tuple)) (new-tuple)) 9))
        (begin (begin (begin
        (begin (< 7 (= 7 (<= 7 (alen (new-array 5 5))))) (new-tuple))
        (begin 8 9)) (begin
        (begin (:eq_arr (new-tuple 1 2 3 4) (new-tuple 1 2 3 4)) 7)
        (begin (make-closure :sq 7) 9)))
        (begin (begin
        (begin (closure-vars (make-closure :sq 7)) (:tolist (new-tuple 1 2 3)))
        (begin (closure-proc (make-closure :sq 7)) 9)) (begin
        (begin 6 7)
        (begin 8 (:identity (let ([x (:double (:sq 3))]) x)))))))
      )
      18
    )""")

  def program(e:String) = "(" + e + "\n" + library + ")"

  def library = """
  (:tolist (arr) (:tolisthelper arr 0))
  (:tolisthelper (arr index)
    (if
      (<= (alen arr) index)
      (:nil)
      (:cons (aref arr index) (:tolisthelper arr (+ 1 index)))
    )
  )
  (:foreach (list f)
    (if (:empty list)
      1
      (begin (f (:head list)) (:foreach (:tail list) f))
    )
  )
  (:map (list f)
    (if (:empty list)
      (:nil)
      (:cons (f (:head list)) (:map (:tail list) f))
    )
  )
  (:filter (list p)
    (if (:empty list)
      (new-tuple)
      (if (p (:head list))
        (:cons (:head list) (:filter (:tail list) p))
        (:filter (:tail list) p)
      )
    )
  )
  (:foldl (f init list)
    (if (:empty list)
      init
      (:foldl f (f init (:head list)) (:tail list))
    )
  )
  (:zip (list1 list2)
    (if (:empty list1)
      (:nil)
      (if (:empty list2)
        (:nil)
        (:cons (:cons (:head list1) (:cons (:head list2) (:nil))) (:zip (:tail list1) (:tail list2)))
      )
    )
  )
  (:find (list p)
    (if (:empty list)
      (:none)
      (if (p (:head list))
        (:some (:head list))
        (:find (:tail list) p)
      )
    )
  )
  (:sq (x) (* x x))
  (:double (x) (+ x x))
  (:sqdouble (x) (* (:double x) (:double x)))
  (:greaterthan3 (x) (< 3 x))
  (:add (x y) (+ x y))
  (:mult (x y) (* x y))
  (:some (x) (:tolist (new-tuple x)))
  (:none () (new-tuple))
  (:option_get (op) (aref op 0))
  (:nil () (new-tuple))
  (:cons (x list) (new-tuple x list))
  (:head (list) (aref list 0))
  (:tail (list) (aref list 1))
  (:empty (list) (= 0 (alen list)))
  (:size (list) (if (:empty list) 0 (+ 1 (:size (:tail list)))))
  (:contains (item list)
    (if (:empty list)
      0
      (if (= item (:head list))
        1
        (:contains item (:tail list))
      )
    )
  )
  (:not (x) (if x 0 1))
  (:and (x y) (if x (if y 1 0) 0))
  (:or (x y) (if x 1 (if y 1 0)))
  (:eq (x y)
    (if (:and (a? x) (a? y))
      (:eq_list x y)
      (if (:and (number? x) (number? y))
        (= x y)
        0
      )
    )
  )
  (:eq_num (x y) (= x y))
  (:eq_list (x y)
    (if (:and (:empty x) (:empty y))
      1
      (if (:or (:empty x) (:empty y))
        0
        (:and (:eq (:head x) (:head y)) (:eq_list (:tail x) (:tail y)))
      )
    )
  )
  (:eq_arr (x y)
    (if (= (alen x) (alen y))
      (:eq_arr_helper x y 0)
      0
    )
  )
  (:eq_arr_helper (x y index)
    (if (:and (= (alen x) index) (= (alen y) index))
      1
      (:and
        (:eq (aref x index) (aref y index))
        (:eq_arr_helper x y (+ 1 index))
      )
    )
  )
  (:test (actual expected)
    (if (:eq actual expected)
      (:print 1)
      (begin (:print actual) (:print expected))
    )
  )
  (:print (x) (print x))
  (:identity (x) x)
  """


  def libraryeasy = """
  (:test (actual expected)
    (if (:eq actual expected)
      (print 1)
      (begin (print actual) (print expected))
    )
  )
  (:not (x) (if x 0 1))
  (:and (x y) (if x (if y 1 0) 0))
  (:or (x y) (if x 1 (if y 1 0)))
  (:eq (x y)
    (if (:and (a? x) (a? y))
      (:eq_list x y)
      (if (:and (number? x) (number? y))
        (= x y)
        0
      )
    )
  )
  (:eq_list (x y)
    (if (:and (:empty x) (:empty y))
      1
      (if (:or (:empty x) (:empty y))
        0
        (:and (:eq (:head x) (:head y)) (:eq_list (:tail x) (:tail y)))
      )
    )
  )
  (:nil () (new-tuple))
  (:cons (x list) (new-tuple x list))
  (:head (list) (aref list 0))
  (:tail (list) (aref list 1))
  (:empty (list) (= 0 (alen list)))
  (:size (list) (if (:empty list) 0 (+ 1 (:size (:tail list)))))
"""

  import io.FileHelper._
  new java.io.File("./test/4-test").mkdir()
  lazy val testcount = Iterator.from(0)

  def testCompile(L4E:String, extraFunctions: String = "") = {
    val index = testcount.next()
    test(index + "-" + L4E){
      val L4Code = program(L4E + "\n" + extraFunctions)
      // write the test
      new java.io.File("./test/4-test/test" + index + ".L4").write(L4Code.clean)
      // compile the code
      val L3Code = new L4Compiler{}.compileToString(L4Code.clean)
      // write the expected result
      new java.io.File("./test/4-test/test" + index + ".L3").write(L3Code.clean)
      // run the interepters
      val L4InterpResult = L4Interpreter.run(L4Code.clean)
      val L3InterpResult = L3Interpreter.run(L3Code.clean)
      if(L4InterpResult.trim == "") error("nothing happened in: " + L4Code)
      // verify the results
      verboseAssert("L4 vs L3 interps", L4InterpResult, L3InterpResult)
      verboseAssert(L4E, L4InterpResult, "1")
    }
  }

//  testCompileE("((print 5))")
//
//  def testCompileE(L4E:String) = {
//    val index = testcount.next()
//    test(index + "-" + L4E){
//      val L4Code = L4E
//      // write the test
//      new java.io.File("./test/4-test/test" + index + ".L4").write(L4Code.clean)
//      // compile the code
//      val L3Code = new L4Compiler{}.compileToString(L4Code.clean)
//      // write the expected result
//      new java.io.File("./test/4-test/test" + index + ".L3").write(L3Code.clean)
//      // run the interepters
//      val L4InterpResult = L4Interpreter.run(L4Code.clean)
//      val L3InterpResult = L3Interpreter.run(L3Code.clean)
//      if(L4InterpResult.trim == "") error("nothing happened in: " + L4Code)
//      // verify the results
//      verboseAssert("L4 vs L3 interps", L4InterpResult, L3InterpResult)
//    }
//  }
}

class L4CompileETests extends TestHelpers{

  compileETest("7", "7")
  compileETest("(b c)", "(b c)")
  compileETest("(a b c)", "(a b c)")
  compileETest("(a (b c))", "(let ([__x0 (b c)]) (a __x0))")
  compileETest("(a (b (c d)))", "(let ([__x0 (c d)]) (let ([__x1 (b __x0)]) (a __x1)))")
  compileETest("(a (b (c d)) (x (y z)))",
    "(let ([__x0 (c d)]) (let ([__x1 (b __x0)]) (let ([__x2 (y z)]) (let ([__x3 (x __x2)]) (a __x1 __x3)))))")
  compileETest("(a (b (c d)) 7 (x (y z)))",
    "(let ([__x0 (c d)]) (let ([__x1 (b __x0)]) (let ([__x2 (y z)]) (let ([__x3 (x __x2)]) (a __x1 7 __x3)))))")
  compileETest("(let ([a 8]) 7)", "(let ([a 8]) 7)")
  compileETest("(let ([a (b c)]) 7)", "(let ([a (b c)]) 7)")
  compileETest("(let ([a (b (c d))]) 7)", "(let ([__x0 (c d)]) (let ([a (b __x0)]) 7))")
  compileETest("(print (a b))", "(let ([__x0 (a b)]) (print __x0))")
  compileETest("(+ a b)", "(+ a b)")
  compileETest("(+ (a b) c)", "(let ([__x0 (a b)]) (+ __x0 c))")
  compileETest("(+ c (a b))", "(let ([__x0 (a b)]) (+ c __x0))")
  compileETest("(+ (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (+ __x0 __x1)))")
  compileETest("(- a b)", "(- a b)")
  compileETest("(- (a b) c)", "(let ([__x0 (a b)]) (- __x0 c))")
  compileETest("(- c (a b))", "(let ([__x0 (a b)]) (- c __x0))")
  compileETest("(- (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (- __x0 __x1)))")
  compileETest("(* a b)", "(* a b)")
  compileETest("(* (a b) c)", "(let ([__x0 (a b)]) (* __x0 c))")
  compileETest("(* c (a b))", "(let ([__x0 (a b)]) (* c __x0))")
  compileETest("(* (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (* __x0 __x1)))")
  compileETest("(< a b)", "(< a b)")
  compileETest("(< (a b) c)", "(let ([__x0 (a b)]) (< __x0 c))")
  compileETest("(< c (a b))", "(let ([__x0 (a b)]) (< c __x0))")
  compileETest("(< (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (< __x0 __x1)))")
  compileETest("(<= a b)", "(<= a b)")
  compileETest("(<= (a b) c)", "(let ([__x0 (a b)]) (<= __x0 c))")
  compileETest("(<= c (a b))", "(let ([__x0 (a b)]) (<= c __x0))")
  compileETest("(<= (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (<= __x0 __x1)))")
  compileETest("(= a b)", "(= a b)")
  compileETest("(= (a b) c)", "(let ([__x0 (a b)]) (= __x0 c))")
  compileETest("(= c (a b))", "(let ([__x0 (a b)]) (= c __x0))")
  compileETest("(= (a b) (c d))", "(let ([__x0 (a b)]) (let ([__x1 (c d)]) (= __x0 __x1)))")
  compileETest("((b c) (+ (- c d) (* 1 2)))",
    "(let ([__x0 (b c)]) (let ([__x1 (- c d)]) (let ([__x2 (* 1 2)]) (let ([__x3 (+ __x1 __x2)]) (__x0 __x3)))))")
  compileETest("(number? 7)", "(number? 7)")
  compileETest("(number? c)", "(number? c)")
  compileETest("(number? (b c))", "(let ([__x0 (b c)]) (number? __x0))")
  compileETest("(a? 7)", "(a? 7)")
  compileETest("(a? c)", "(a? c)")
  compileETest("(a? (b c))", "(let ([__x0 (b c)]) (a? __x0))")

  compileETest("(new-tuple a b c d e f g h i j k)", "(new-tuple a b c d e f g h i j k)")

  compileETest("(new-tuple a b c (+ d d) e f g h i j k)", "(let ([__x0 (+ d d)]) (new-tuple a b c __x0 e f g h i j k))")
  compileETest("(new-tuple a b c d e f g h i j (+ k k))", "(let ([__x0 (+ k k)]) (new-tuple a b c d e f g h i j __x0))")

  compileETest("(begin x y)", "(let ([__x0 x]) y)")
  compileETest("(begin (+ 1 2) y)", "(let ([__x0 (+ 1 2)]) y)")
  compileETest("(begin y (+ 1 2))", "(let ([__x0 y]) (+ 1 2))")
  compileETest("(begin (+ 1 2) (+ 1 2))", "(let ([__x0 (+ 1 2)]) (+ 1 2))")
  compileETest("(begin (a (b c)) 7)", "(let ([__x1 (b c)]) (let ([__x0 (a __x1)]) 7))")

  // these are just to show that what the begin code translates to is good.
  compileETest("(let ([__x0 (+ 1 2)]) y)", "(let ([__x0 (+ 1 2)]) y)")
  compileETest("(let ([__x0 y]) (+ 1 2))", "(let ([__x0 y]) (+ 1 2))")
  compileETest("(let ([__x0 (+ 1 2)]) (+ 1 2))", "(let ([__x0 (+ 1 2)]) (+ 1 2))")
  compileETest("(let ([__x1 (b c)]) (let ([__x0 (a __x1)]) 7))", "(let ([__x1 (b c)]) (let ([__x0 (a __x1)]) 7))")

  //(let ([passed (aref 0 results))) (new-tuple (if pass? (+ 1 passed) passed) (+ 1 (aref 1 results))))
  compileETest("""
(new-tuple
(let ([passed (aref 0 results)])
(if pass? (+ 1 passed) passed))
(+ 1 (aref 1 results)))""",
"""
(let ([passed (aref 0 results)])
(if pass? (let ([__x3 (+ 1 passed)])
(let ([__x4 (new-tuple results)])
(:__f0 __x3 __x4)))
(let ([__x5 (new-tuple results)])
(:__f0 passed __x5))))
(:__f0 (__x0 frees)
(let ([results (aref frees 0)])
(let ([__x1 (aref 1 results)])
(let ([__x2 (+ 1 __x1)])
(new-tuple __x0 __x2)))))
""")

  // this demonstrates the let renaming error from
  // http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture09.pdf (pg 25)
  // c is free outside the let, but becomes captured by it!
  compileETest("((let ([c a]) (b c)) c)", "(let ([c a]) (let ([__x0 (b c)]) (__x0 c)))", changeVarNames = false)
  // to avoid this, we rename all the variables first
  compileETest("((let ([d a]) (b d)) d)", "(let ([__x0 a]) (let ([__x1 (b __x0)]) (__x1 d)))",
    changeVarNames = true)

  def compileETest(code:String, expected:String, changeVarNames:Boolean = false) = {
    test(code.clean){
      val res = compileE(code.clean, changeVarNames = changeVarNames)
      //println("compileETest(\"" + code + "\", " + "\"" + L4Printer.toCode(res) + "\")")
      verboseAssert(code, res.clean.replace("\n", " "), expected.clean.replace("\n", " "))
    }
  }

  def compileE(code:String, changeVarNames:Boolean = true, allowFrees:Boolean = false): String = {
    val compiler = new L4Compiler{}
    import compiler._
    val ast = parseE(read(code))
    val (e, more) = if(changeVarNames) find(changeVarNamesInE(ast, allowFrees = true)) else find(ast)
    L3Printer.toCode(e) + (if(more.isEmpty) "" else "\n" + more.map(L3Printer.toCode).mkString("\n"))
  }
}


class ChangeVarNamesTests extends TestHelpers {

  // renames the let bound c only. the rest are free and this call allows frees.
  changeVarNamesInE("((let ([c a]) (b c)) c)", "((let ([__x0 a]) (b __x0)) c)", allowFrees = true)

  changeVarNamesInE("(let ([c 7]) (print c))", "(let ([__x0 7]) (print __x0))")

  changeVarNamesInE(
    "(let ([c 7]) (let ([c 7]) (print c)))",
    "(let ([__x0 7]) (let ([__x1 7]) (print __x1)))")

  changeVarNamesInE(
    "(let ([c 7]) (begin (let ([c 7]) (print c)) (print c)))",
    "(let ([__x0 7]) (begin (let ([__x1 7]) (print __x1)) (print __x0)))")

  changeVarNamesInE(
    "(let ([c 7]) (begin (let ([c c]) (print c)) (print c)))",
    "(let ([__x0 7]) (begin (let ([__x1 __x0]) (print __x1)) (print __x0)))")

  changeVarNamesInFunc(
    "(:f (x) (let ([c x]) (begin (let ([c x]) (print c)) (print c))))",
    "(:f (__x0) (let ([__x1 __x0]) (begin (let ([__x2 __x0]) (print __x2)) (print __x1))))")

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


/**
def library = """
(:tolist (arr) (:tolisthelper arr 0))
(:tolisthelper (arr index)
  (if
    (<= (alen arr) index)
    (:nil)
    (:cons (aref arr index) (:tolisthelper arr (+ 1 index)))
  )
)
(:foreach (list f)
  (if (:empty list)
    1
    (begin (f (:head list)) (:foreach (:tail list) f))
  )
)
(:map (list f)
  (if (:empty list)
    (:nil)
    (:cons (f (:head list)) (:map (:tail list) f))
  )
)
(:filter (list p)
  (if (:empty list)
    (new-tuple)
    (if (p (:head list))
      (:cons (:head list) (:filter (:tail list) p))
      (:filter (:tail list) p)
    )
  )
)
(:foldl (f init list)
  (if (:empty list)
    init
    (:foldl f (f init (:head list)) (:tail list))
  )
)
(:zip (list1 list2)
  (if (:empty list1)
    (:nil)
    (if (:empty list2)
      (:nil)
      (:cons (:cons (:head list1) (:cons (:head list2) (:nil))) (:zip (:tail list1) (:tail list2)))
    )
  )
)
(:find (list p)
  (if (:empty list)
    (:none)
    (if (p (:head list))
      (:some (:head list))
      (:find (:tail list) p)
    )
  )
)
(:sq (x) (* x x))
(:double (x) (+ x x))
(:sqdouble (x) (* (:double x) (:double x)))
(:greaterthan3 (x) (< 3 x))
(:add (x y) (+ x y))
(:mult (x y) (* x y))
(:some (x) (:tolist (new-tuple x)))
(:none () (new-tuple))
(:option_get (op) (aref op 0))
(:nil () (new-tuple))
(:cons (x list) (new-tuple x list))
(:head (list) (aref list 0))
(:tail (list) (aref list 1))
(:empty (list) (= 0 (alen list)))
(:size (list) (if (:empty list) 0 (+ 1 (:size (:tail list)))))
(:contains (item list)
  (if (:empty list)
    0
    (if (= item (:head list))
      1
      (:contains item (:tail list))
    )
  )
)
(:not (x) (if x 0 1))
(:and (x y) (if x (if y 1 0) 0))
(:or (x y) (if x 1 (if y 1 0)))
(:eq (x y)
  (if (:and (a? x) (a? y))
    (:eq_list x y)
    (if (:and (number? x) (number? y))
      (= x y)
      0
    )
  )
)
(:eq_num (x y) (= x y))
(:eq_list (x y)
  (if (:and (:empty x) (:empty y))
    1
    (if (:or (:empty x) (:empty y))
      0
      (:and (:eq (:head x) (:head y)) (:eq_list (:tail x) (:tail y)))
    )
  )
)
(:eq_arr (x y)
  (if (= (alen x) (alen y))
    (:eq_arr_helper x y 0)
    0
  )
)
(:eq_arr_helper (x y index)
  (if (:and (= (alen x) index) (= (alen y) index))
    1
    (:and
      (:eq (aref x index) (aref y index))
      (:eq_arr_helper x y (+ 1 index))
    )
  )
)
(:test (actual expected)
  (if (:eq actual expected)
    (:print 1)
    (begin (:print actual) (:print expected))
  )
)
(:print (x) (print x))
(:identity (x) x)
"""
**/