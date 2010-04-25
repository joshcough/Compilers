package L2Compiler

object Graph{
  def apply[T](conns:TwoWayConnection[T]*): Graph[T] = new Graph[T](Set()) ++ conns.toList
  implicit def anyToConnection[T](src:T) = new {
    def <->[T](dest:T) = TwoWayConnection(src, dest)
  }
}
case class TwoWayConnection[T](n1:T, n2:T){
  override def toString = n1 + "<->" + n2
}
case class Graph[T](connections:Set[TwoWayConnection[T]]){
  def ++(cons: List[TwoWayConnection[T]]): Graph[T] = cons match {
    case c::cs => (this + c) ++ cs
    case _ => this
  }
  def + (con: TwoWayConnection[T]) = {
    Graph(connections + con + TwoWayConnection(con.n2, con.n1))
  }
}