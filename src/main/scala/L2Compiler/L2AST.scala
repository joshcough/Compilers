package L2Compiler

object L2AST {
  import L1Compiler.L1AST._
  object L2{ def apply(main: Func): L2 = L2(main, Nil) }
  case class L2(main: Func, funs:List[Func])

  case class Variable(val name: String) extends X {
    override def toString = "Variable(\"" + name + "\")"
    def toL2Code: String = name
  }
}
