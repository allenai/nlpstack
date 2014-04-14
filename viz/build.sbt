name := "nlpviz"

libraryDependencies ++= Seq(
  // server
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "io.spray" % "spray-can" % sprayVersion,
  "io.spray" % "spray-routing" % sprayVersion,
  // config
  "com.typesafe" % "config" % "1.2.0",
  // vizualization
  "org.riedelcastro" % "whatswrong" % "0.2.4",
  // nlp
  "org.allenai.nlptools" %% "nlptools-core" % "2.5.0-SNAPSHOT") ++ loggingImplementations

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.slf4j" % "slf4j-api" % "1.7.6")
