package L2Compiler

object Graph{
  def apply(conns:TwoWayConnection*): Graph = new Graph(Set()) ++ conns.toList
  implicit def stringToConnection(src:String) = new {
    def <->(dest:String) = TwoWayConnection(src, dest)
  }
}
case class TwoWayConnection(n1:String, n2:String){
  override def toString = n1 + "<->" + n2
}
case class Graph(connections:Set[TwoWayConnection]){
  def ++(cons: List[TwoWayConnection]): Graph = cons match {
    case c::cs => (this + c) ++ cs
    case _ => this
  }
  def + (con: TwoWayConnection) = {
    Graph(connections + con + TwoWayConnection(con.n2, con.n1))
  }
}