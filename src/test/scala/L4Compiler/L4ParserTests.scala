package L4Compiler

import util.TestHelpers

import io.Dir._
import io.FileHelper._

class L4ParserTests extends TestHelpers with L4Compiler {

  for(f<-L4TestFest2010Tests) {
    test("parsing: " + f.getAbsolutePath) {
      val code = read(f.read)
      verboseAssert(f.getAbsolutePath, printSExp(read(toCode(parse(code)))), printSExp(code))
    }
  }
}
