package homework

import HW2._

class HW2Test extends HWBaseTest[WAE, Int](HW2Interpreter){

  //////////////////////////
  // interpreter tests
  //////////////////////////
  testCode(""" 12 """ -> 12)
  testCode(""" (+ 12 12) """ -> 24)
  testCode(""" (- 12 12) """ -> 0)
  testCode(""" (+ (+ 5 7) 10) """ -> 22)
  testCode(""" (+ (+ 5 7) (+ 6 3)) """ -> 21)
  testCode(""" (- (- 5 7) (- 6 3)) """ -> -5)

  testCode(""" (with (x 5) x) """ -> 5)
  testCode(""" (with (x 5) (with (x 7) x)) """ -> 7)
  testCode(""" (with (x 5) (with (y 7) (+ x y))) """ -> 12)
  testCode(""" (with (x 5) (with (y 7) (+ (with (z x) (+ z x)) y))) """ -> 17)
  testCode(""" (with (x 5) (with (y 7) (- (with (z x) (+ z x)) y))) """ -> 3)

  //////////////////////////////
  // test cases for free ids
  //////////////////////////////
  def testFreeIds(t:(String,List[Symbol])) = {
    test("free ids: " + t._1 + " => " + t._2){ HW2.freeIds(parse(read(t._1))) === t._2 }
  }

  //trivial cases
  testFreeIds("12" -> Nil)
  testFreeIds("a" -> List(Symbol("a")))

  // simple add tests
  testFreeIds("(+ 7 8)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(+ a 7)" -> List(Symbol("a")))
  testFreeIds("(+ 7 b)" -> List(Symbol("b")))
  testFreeIds("(+ a b)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(+ b a)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(+ z (+ b a))" -> List(Symbol("a"), Symbol("b"), Symbol("z")))

  // simple sub tests are exactly the same as add (its debatable that they are needed)
  testFreeIds("(- 7 8)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(- a 7)" -> List(Symbol("a")))
  testFreeIds("(- 7 b)" -> List(Symbol("b")))
  testFreeIds("(- a b)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(- b a)" -> List(Symbol("a"), Symbol("b")))
  testFreeIds("(- z (- b a))" -> List(Symbol("a"), Symbol("b"), Symbol("z")))

  //simple with tests
  testFreeIds("(with (a 7) a)", Nil)
  testFreeIds("(with (a 7) 8)", Nil)
  testFreeIds("(with (a 7) b)", List(Symbol("b"))) // b free in body expr
  testFreeIds("(with (a b) a)", List(Symbol("b"))) // b free in named expr
  testFreeIds("(with (a b) b)", List(Symbol("b"))) // b free in named expr and body expr
  testFreeIds("(with (a a) 7)", List(Symbol("a"))) // a free in named expr, same name as name being bound
  testFreeIds("(with (a a) a)", List(Symbol("a"))) // a free in named expr same name as name being bound, bound body expr

  // nested with tests
  testFreeIds("(with (a (with (b 7) 8)) a)", Nil) // nested with expr, no free ids
  testFreeIds("(with (a (with (b 7) a)) a)", List(Symbol("a"))) // a free in body of inner with expr
  testFreeIds("(with (a (with (b (+ c d)) b)) a)", List(Symbol("c"), Symbol("d"))) // c and d free in named expr of inner with expr
  //add a few more tests here ppppppppppppppppppppppppppppppppppppppppppp

  ///////////////////////////////
  // test cases for binding ids
  //////////////////////////////
  def testBindingIds(t:(String,List[Symbol])) = {
    test("binding ids: " + t._1 + " => " + t._2){ HW2.bindingIds(parse(read(t._1))) === t._2 }
  }

  // trivial cases
  testBindingIds("7", Nil)
  testBindingIds("a", Nil)
  
  // simple add tests - no binding ids at all, because no with expressions
  testBindingIds("(+ 7 8)", Nil)
  testBindingIds("(+ a 7)", Nil)
  testBindingIds("(+ 7 b)", Nil)
  testBindingIds("(+ a b)", Nil)
  testBindingIds("(+ b a)", Nil)
  testBindingIds("(+ z (+ b a))", Nil)
  
  // simple with tests
  testBindingIds("(with (a 6) b)", List(Symbol("a")))
  testBindingIds("(with (a a) b)", List(Symbol("a")))
  testBindingIds("(with (a b) a)", List(Symbol("a")))
  testBindingIds("(with (a a) a)", List(Symbol("a")))
  testBindingIds("(with (a 7) a)", List(Symbol("a")))
  testBindingIds("(with (a 7) 8)", List(Symbol("a")))
  testBindingIds("(with (a b) b)", List(Symbol("a")))
  testBindingIds("(with (a a) 7)", List(Symbol("a")))
  
  // nested with tests
  testBindingIds("(with (a (with (b 7) 8)) a)", List(Symbol("a"), Symbol("b")))
  testBindingIds("(with (a (with (b 7) a)) a)", List(Symbol("a"), Symbol("b")))
  //////////add a few more tests here ppppppppppppppppppppppppppppppppppppppppppp
  
  // add tests with inner with expressions
  testBindingIds("(+ 7 (with (a 6) b))", List(Symbol("a")))

  /////////////////////////////////
  // test cases for bound-ids
  /////////////////////////////////
  def testBoundIds(t:(String,List[Symbol])) = {
    test("bound ids: " + t._1 + " => " + t._2){ HW2.boundIds(parse(read(t._1))) === t._2 }
  }

  // trivial cases
  testBoundIds("7", Nil)
  testBoundIds("a", Nil)

  // simple add tests
  testBoundIds("(+ 7 8)", Nil)
  testBoundIds("(+ a 7)", Nil)
  testBoundIds("(+ 7 b)", Nil)
  testBoundIds("(+ a b)", Nil)
  testBoundIds("(+ b a)", Nil)
  testBoundIds("(+ z (+ b a))", Nil)

  // simple with tests
  testBoundIds("(with (a 7) a)", List(Symbol("a")))
  testBoundIds("(with (a 7) 8)", List(Symbol("a")))
  testBoundIds("(with (a 7) b)", List(Symbol("a"))) // b free in body expr
  testBoundIds("(with (a b) a)", List(Symbol("a"))) // b free in named expr
  testBoundIds("(with (a b) b)", List(Symbol("a"))) // b free in named expr and body expr
  testBoundIds("(with (a a) 7)", List(Symbol("a"))) // a free in named expr, same name as name being bound
  testBoundIds("(with (a a) a)", List(Symbol("a"))) // a free in named expr same name as name being bound, bound body expr

  // nested with tests
  testBoundIds("(with (a (with (b 7) 8)) a)", List(Symbol("a"), Symbol("b"))) // nested with expr, no free ids
  testBoundIds("(with (a (with (b 7) a)) a)", List(Symbol("a"), Symbol("b"))) // a free in body of inner with expr
  testBoundIds("(with (a (with (b (+ c d)) b)) a)", List(Symbol("a"), Symbol("b"))) // c and d free in named expr of inner with expr
  testBoundIds("(with (a (with (b (+ c d)) b)) (with (z a) (+ z z)))", List(Symbol("a"), Symbol("b"), Symbol("z")))
  testBoundIds("(with (z (with (a (+ c d)) b)) (with (b a) (+ z z)))", List(Symbol("a"), Symbol("b"), Symbol("z")))
  testBoundIds("(with (z (with (z (+ c d)) b)) (with (z a) (+ z z)))", List(Symbol("z")))
  //////////add a few more tests here ppppppppppppppppppppppppppppppppppppppppppp

}
