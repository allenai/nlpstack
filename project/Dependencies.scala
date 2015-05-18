import sbt._
import org.allenai.plugins.CoreDependencies

object Dependencies extends CoreDependencies {
  val datastore = "org.allenai" %% "datastore" % "2015.01.23-0"

  val commonsIo = "commons-io" % "commons-io" % "2.4"

  val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

  val clearGroup = "com.clearnlp"
  val clearVersion = "2.0.2"
  val clear = clearGroup % "clearnlp" % clearVersion
  val opennlp = ("org.apache.opennlp" % "opennlp-tools" % "1.5.3"
    exclude ("net.sf.jwordnet", "jwnl"))

  val stanfordCoreNlp = "edu.stanford.nlp" % "stanford-corenlp" % "3.4.1"

  val factorie = ("cc.factorie" %% "factorie" % "1.1.1"
    exclude ("junit", "junit")
    exclude ("commons-logging", "commons-logging"))
  val factorieWordnet = "cc.factorie.app.nlp" % "wordnet" % "1.0"

  val testingLibraries = Seq(allenAiTestkit % "test")

  val apache2 = "Apache 2.0 " -> url("http://www.opensource.org/licenses/bsd-3-clause")

  val loggingDependencies = Seq(
    Logging.slf4jApi,
    Logging.logbackCore,
    Logging.logbackClassic,
    "org.slf4j" % "jcl-over-slf4j" % Logging.slf4jVersion,
    "org.slf4j" % "log4j-over-slf4j" % Logging.slf4jVersion,
    "org.slf4j" % "jul-to-slf4j" % Logging.slf4jVersion
  )

  val jVerbnet = "edu.mit" % "jverbnet" % "1.2.0.1"

  val reming = "com.github.jkinkead" %% "reming-json" % "0.0.9"

  val Overrides = loggingDependencies.toSet
}
