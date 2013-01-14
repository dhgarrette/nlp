name := "hmm"

version := "1.0.0"

organization := "dhg"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % "7.0.0-M7",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
