name := "TypeClassDebug"
version := "0.1"
val scala = "2.13.2"
scalaVersion := scala

libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-compiler" % scala,
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

libraryDependencies in ThisBuild ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.13.0" % "test")