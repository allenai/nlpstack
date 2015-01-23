import Dependencies._

name := "webapp"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  "org.riedelcastro" % "whatswrong" % "0.2.4"
)

dependencyOverrides += "commons-io" % "commons-io" % "2.4"

addLoggingDependencies(libraryDependencies)
