import sbt._
import Keys._

object CcgBuild extends Build {

  lazy val main = Project("ccg", file(".")) dependsOn(scalautil)

  lazy val scalautil = Project("scala-util", file("scala-util"))

}

