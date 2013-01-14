name := "nlp"

version := "1.0.0"

organization := "dhg"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % "7.0.0-M7",
  "org.scalanlp" % "breeze-learn_2.10" % "0.2-SNAPSHOT" changing(),
  "org.scalanlp" % "breeze-viz_2.10" % "0.2-SNAPSHOT" changing(),
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
