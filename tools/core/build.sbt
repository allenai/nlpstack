import Dependencies._

name := "nlpstack-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    parserCombinators,
    // for remotes
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.2")

dependencyOverrides ++= Set(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2")

libraryDependencies ++= loggingDependencies
