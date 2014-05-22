addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

resolvers += "allenai nexus repository" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"

resolvers += "allenai nexus repository snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"

credentials += Credentials("Sonatype Nexus Repository Manager", "utility.allenai.org", "deployment", "answermyquery")

addSbtPlugin("org.allenai.plugins" % "sbt-deploy" % "2014.4.14-1-SNAPSHOT")
