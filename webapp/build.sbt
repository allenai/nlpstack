import Dependencies._
import com.typesafe.sbt.SbtNativePackager.NativePackagerHelper._

name := "webapp"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  "org.riedelcastro" % "whatswrong" % "0.2.4"
)
