import Dependencies._
import com.typesafe.sbt.SbtNativePackager.NativePackagerHelper._

name := "webapp"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  // server
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  sprayModule("can"),
  sprayModule("routing"),
  "io.spray" %% "spray-json" % "1.2.6",
  // config
  "com.typesafe" % "config" % "1.2.0",
  // vizualization
  "org.riedelcastro" % "whatswrong" % "0.2.4") ++ loggingImplementations

dependencyOverrides ++= Set(slf4j)

mappings in Universal ++= directory(baseDirectory.value / "public")
