import sbt._
import org.allenai.plugins.CoreDependencies

object Dependencies extends CoreDependencies {
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.7"
  val logbackVersion = "1.1.2"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImplementations = Seq(logbackCore, logbackClassic)
  val commonsIo = "commons-io" % "commons-io" % "2.4"

  val clearGroup = "com.clearnlp"
  val clearVersion = "2.0.2"
  val clear = clearGroup % "clearnlp" % clearVersion
  val opennlp = ("org.apache.opennlp" % "opennlp-tools" % "1.5.3"
    exclude("net.sf.jwordnet", "jwnl"))

  val factorie = ("cc.factorie" % "factorie" % "1.0"
    exclude("com.typesafe.akka", "akka-actor_2.10")
    exclude("org.scala-lang", "scala-reflect")
    exclude("com.thoughtworks.paranamer", "paranamer")
    exclude("com.google.guava", "guava")
    exclude("junit", "junit"))
  val factoriePosModel = "cc.factorie.app.nlp.pos" % "OntonotesForwardPosTaggerModel" % "1.0"
  val factorieParseModel = "cc.factorie.app.nlp.parse" % "OntonotesTransitionBasedParserModel" % "1.0"
  val factorieWordnet = "cc.factorie.app.nlp" % "wordnet" % "1.0"
  val factorieLexicon = "cc.factorie.app.nlp" % "lexicon" % "1.0"
  val factorieCorefModel = "cc.factorie.app.nlp.coref" % "ParseStructuredCorefModel" % "1.0"
  val factorieNerModel = "cc.factorie.app.nlp.ner" % "ConllChainNerModel" % "1.0"
  val factoriePhraseModel = "cc.factorie.app.nlp.phrase" % "OntonotesPhraseEntityTypeLabelerModel" % "1.0"

  val testingLibraries = Seq(allenAiTestkit % "test")

  val apache2 = "Apache 2.0 " -> url("http://www.opensource.org/licenses/bsd-3-clause")
}
