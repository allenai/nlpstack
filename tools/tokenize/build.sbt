import Dependencies._

javaOptions in (Test, test) := Seq("-Xss1m")

fork in (Test, test) := true

addLoggingDependencies(libraryDependencies)
