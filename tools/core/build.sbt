import Dependencies._

name := "nlpstack-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    parserCombinators,
    allenAiCommon,
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
    slf4j)
