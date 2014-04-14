name := "aitk-tools-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    allenAiCommon,
    slf4j,
    // standalone scope
    scopt % "provided",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion % "provied",
    "io.spray" % "spray-can" % sprayVersion % "provided",
    "io.spray" % "spray-routing" % sprayVersion % "provided",
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0") ++ testingLibraries

dependencyOverrides ++= Set("org.slf4j" % "slf4j-api" % "1.7.6")
