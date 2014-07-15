import sbt._

object Dependencies {
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.6"
  val logbackVersion = "1.1.1"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImplementations = Seq(logbackCore, logbackClassic)

  val allenAiCommon = "org.allenai.common" %% "common" % "2014.04.28-SNAPSHOT"
  val allenAiTestkit = "org.allenai.common" %% "testkit" % "0.0.3-SNAPSHOT"

  val clearGroup = "com.clearnlp"
  val clearVersion = "2.0.2"
  val clear = clearGroup % "clearnlp" % clearVersion
  val opennlp = ("org.apache.opennlp" % "opennlp-tools" % "1.5.3"
    exclude("net.sf.jwordnet", "jwnl"))

  val factorie = ("cc.factorie" % "factorie" % "1.0"
    exclude("com.typesafe.akka", "akka-actor_2.10")
    exclude("org.scala-lang", "scala-reflect")
    exclude("com.thoughtworks.paranamer", "paranamer")
    exclude("com.google.guava", "guava"))
  val factoriePosModel = "cc.factorie.app.nlp.pos" % "OntonotesForwardPosTaggerModel" % "1.0"
  val factorieParseModel = "cc.factorie.app.nlp.parse" % "OntonotesTransitionBasedParserModel" % "1.0"
  val factorieWordnet = "cc.factorie.app.nlp" % "wordnet" % "1.0"

  val testingLibraries = Seq(allenAiTestkit % "test")

  val scopt = "com.github.scopt" %% "scopt" % "3.2.0"
  val typesafeConfig = "com.typesafe" % "config" % "1.2.0"

  val sprayVersion = "1.3.1"
  val akkaVersion = "2.3.2"

  val apache2 = "Apache 2.0 " -> url("http://www.opensource.org/licenses/bsd-3-clause")

}
