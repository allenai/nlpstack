name := "nlpstack-core"

licenses := Seq(apache2)

libraryDependencies ++= Seq(
    allenAiCommon,
    slf4j,
    // standalone scope
    scopt,
    typesafeConfig,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "io.spray" % "spray-can" % sprayVersion,
    "io.spray" % "spray-routing" % sprayVersion,
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0")

dependencyOverrides ++= Set("org.slf4j" % "slf4j-api" % "1.7.6")
