package interpreter

import interpreter.LazyInterpreter._

class LazyInterpreterTest extends LazyInterpreterBaseTest{
  
  test("simple add", Add(Num(5), Num(10)) -> NumValue(15))
  test("simple apply", Application(Func('x, Id('x)), Num(5)) -> NumValue(5))

  val addFunction = Func('x, Func('y, Add(Id('x), Id('y))))

  test("eval a function", addFunction -> Closure('x,Func('y,Add(Id('x),Id('y))),Nil))

  test("return a function",
    Application(addFunction, Num(10)) -> Closure('y,Add(Id('x),Id('y)),List(('x,ExpClosure(Num(10),Nil)))))

  test("double apply", Application(Application(addFunction, Num(10)), Num(4)) -> NumValue(14))

}