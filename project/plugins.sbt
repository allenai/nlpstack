lazy val ai2PluginsVersion = "2014.11.05-0"

// will be added to all projects automatically:
addSbtPlugin("org.allenai.plugins" % "allenai-sbt-core-settings" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-release" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-deploy" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-webapp" % ai2PluginsVersion)
