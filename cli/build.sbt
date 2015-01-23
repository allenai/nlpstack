import Dependencies._

name := "nlpstack-cli"

libraryDependencies ++= Seq(
  scopt,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  sprayCan,
  sprayRouting,
  typesafeConfig)

addLoggingDependencies(libraryDependencies)
