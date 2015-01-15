import sbt._
import org.allenai.plugins.CoreDependencies

object Dependencies extends CoreDependencies {
  val datastore = "org.allenai" %% "datastore" % "2014.12.03-1"

  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.7"
  val logbackVersion = "1.1.2"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImplementations = Seq(logbackCore, logbackClassic)
  val commonsIo = "commons-io" % "commons-io" % "2.4"

  val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

  val clearGroup = "com.clearnlp"
  val clearVersion = "2.0.2"
  val clear = clearGroup % "clearnlp" % clearVersion
  val opennlp = ("org.apache.opennlp" % "opennlp-tools" % "1.5.3"
    exclude("net.sf.jwordnet", "jwnl"))

  val factorie = ("cc.factorie" %% "factorie" % "1.1.1"
    exclude("org.scala-lang", "scala-reflect")
    exclude("com.thoughtworks.paranamer", "paranamer")
    exclude("com.google.guava", "guava")
    exclude("junit", "junit"))
  val factorieWordnet = "cc.factorie.app.nlp" % "wordnet" % "1.0"

  val testingLibraries = Seq(allenAiTestkit % "test")

  val apache2 = "Apache 2.0 " -> url("http://www.opensource.org/licenses/bsd-3-clause")
}
