Revolver.settings

Deploy.settings

name := "nlpviz"

scalaVersion := "2.10.4"

val logbackVersion = "1.1.1"

val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion

val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion

val loggingImplementations = Seq(logbackCore, logbackClassic)

libraryDependencies ++= Seq(
  // server
  "com.typesafe.akka" %% "akka-actor" % "2.3.2",
  "io.spray" % "spray-can" % "1.3.1",
  "io.spray" % "spray-routing" % "1.3.1",
  // config
  "com.typesafe" % "config" % "1.2.0",
  // vizualization
  "org.riedelcastro" % "whatswrong" % "0.2.4",
  // nlp
  "org.allenai.nlptools" %% "nlptools-core" % "2.5.0-SNAPSHOT") ++ loggingImplementations

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.slf4j" % "slf4j-api" % "1.7.6")

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

conflictManager := ConflictManager.strict
