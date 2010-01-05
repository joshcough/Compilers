package homework

import HW1._

class HW1Test extends HWBaseTest[Exp, String](HW1Interpreter){

  testCode(""" "hello" """ -> "hello")
  testCode(""" (("hello" & " ") & "world") """ -> "hello world")

  testCode(""" (("hello" @ "l") & ("a world" @ "a")) """ -> "lo world")

  testCode(""" ("filename.scm" @ ".") """ -> "scm")

}