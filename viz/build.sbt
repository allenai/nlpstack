Deploy.settings

name := "nlpviz"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  // server
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "io.spray" % "spray-can" % sprayVersion,
  "io.spray" % "spray-routing" % sprayVersion,
  // config
  "com.typesafe" % "config" % "1.2.0",
  // vizualization
  "org.riedelcastro" % "whatswrong" % "0.2.4") ++ loggingImplementations

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.slf4j" % "slf4j-api" % "1.7.6")
