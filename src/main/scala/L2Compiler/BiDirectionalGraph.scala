package L2Compiler

object BiDirectionalGraph{
  def apply[T](conns:(T,T)*): BiDirectionalGraph[T] = {
    new BiDirectionalGraph[T](Set()) ++ conns.toList
  }
}

case class BiDirectionalGraph[T](connections:Set[(T,T)]){
  def ++(cons: List[(T,T)]): BiDirectionalGraph[T] = cons match {
    case c::cs => (this + c) ++ cs
    case _ => this
  }
  def + (con: (T,T)) = new BiDirectionalGraph(connections + con + (con._2 -> con._1))
  def contains(con:(T,T)) = connections.contains(con) || connections.contains(con._2 -> con._1)
  def map[B](f: ((T,T)) => (B,B)) = new BiDirectionalGraph(connections.map(f))
  def filter(p: ((T,T)) => Boolean) = new BiDirectionalGraph(connections.filter(p))
  def find(p: ((T,T)) => Boolean) = connections.find(p)
  def neigborsOf(t:T): Set[T] =
    connections.filter(_._1 == t).map(_._2) union connections.filter(_._2 == t).map(_._1)
  def replace(t1:T, t2:T) = new BiDirectionalGraph(connections.map{
    case (x1,x2) => if(t1 == x1) (t2, x2) else if (t1 == x2) (x1,t2) else (x1,x2)
  })
  def members: Set[T] = connections.flatMap{ p => Set(p._1, p._2) }
}