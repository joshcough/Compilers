trait F
case class FakeHiddenId(name:String) extends F
val Id: (String => F) = s:String => new FakeHiddenId(s)


