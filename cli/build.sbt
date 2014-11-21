import Dependencies._
import com.typesafe.sbt.SbtNativePackager.NativePackagerHelper._

name := "nlpstack-cli"

libraryDependencies ++= Seq(
  scopt,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  sprayModule("can"),
  sprayModule("routing"),
  // config
  typesafeConfig) ++ loggingImplementations

dependencyOverrides ++= Set(
  slf4j)
