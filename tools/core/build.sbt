import Dependencies._

name := "nlpstack-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    allenAiCore,
    slf4j,
    // for remotes
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0")
