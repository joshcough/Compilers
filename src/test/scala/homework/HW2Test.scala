package homework

import HW2._

class HW2Test extends HWBaseTest[WAE, Int](HW2Interpreter){

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


//  testCode(""" (("hello" @ "l") & ("a world" @ "a")) """ -> 12)
//  testCode(""" ("filename.scm" @ ".") """ -> 12)
}