addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

resolvers += "allenai nexus repository" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"

resolvers += "allenai nexus repository snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"

credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "utility.allenai.org",
  "travis",
  System.getenv("NEXUS_PASS"))

resolvers += Resolver.url(
  "allenai-bintray-sbt-plugins",
  url("http://dl.bintray.com/content/allenai/sbt-plugins"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.7.1")

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-deploy" % "2014.07.03-0")

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-format" % "2014.07.03-0")

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-travis-publisher" % "2014.07.03-0")
