import sbt._

class IntepreterProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
}
