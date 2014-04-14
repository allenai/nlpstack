name := "aitk-tools-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    allenAiCommon,
    scopt,
    // standalone scope
    slf4j,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "io.spray" % "spray-can" % sprayVersion,
    "io.spray" % "spray-routing" % sprayVersion,
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0") ++ testingLibraries

dependencyOverrides ++= Set("org.slf4j" % "slf4j-api" % "1.7.6")
