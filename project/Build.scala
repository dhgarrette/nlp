import sbt._
import Keys._

object HmmBuild extends Build {

  lazy val main = Project("hmm", file(".")) dependsOn(scalautil)

  lazy val scalautil = Project("scala-util", file("scala-util"))

}
