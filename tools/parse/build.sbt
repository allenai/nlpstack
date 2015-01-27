import Dependencies._

parallelExecution in ThisBuild := false

javaOptions += "-Xms512M"

javaOptions += "-Xmx2G"

javaOptions += "-XX:ReservedCodeCacheSize=512M"

fork in test := true
