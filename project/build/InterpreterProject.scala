import sbt._


class IntepreterProject(info: ProjectInfo) extends DefaultProject(info) {
  /// repositories
  val scalaToolsRepository = "Scala Tools Repository" at "http://scala-tools.org/repo-snapshots"
  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-RC5-with-test-interfaces-0.2-SNAPSHOT" % "test"
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1-RC5" % "1.7-SNAPSHOT" % "test"
}
