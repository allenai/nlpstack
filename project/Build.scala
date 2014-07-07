import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object NlpstackBuild extends Build {
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

  val aggregateSettings = Defaults.coreDefaultSettings ++
      Seq(
        publishArtifact := false,
        publishTo := Some(Resolver.file("Unused Repository", file("target/unusedrepo"))))

  lazy val root = Project(
    id = "nlpstack-root",
    base = file("."),
    settings = aggregateSettings).aggregate(tools, webapp)

  val buildSettings = Defaults.defaultSettings ++
    Revolver.settings ++
    Deploy.settings ++
    Format.settings ++
    Publish.settings ++
    TravisPublisher.settings ++
    Seq(
      organization := "org.allenai.nlpstack",
      scalaVersion := "2.10.4",
      scalacOptions ++= Seq("-Xlint", "-deprecation", "-feature"),
      conflictManager := ConflictManager.strict,
      resolvers ++= Seq(
              "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots",
              "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
              "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
              "IESL Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"),
      libraryDependencies ++= testingLibraries,
      dependencyOverrides ++= Set(
        "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "commons-io" % "commons-io" % "2.4")
    )

  lazy val tools = Project(
    id = "tools-root",
    base = file("tools"),
    settings = aggregateSettings).aggregate(lemmatize, tokenize, postag, chunk, parse, segment, core)

  lazy val webapp = Project(
    id = "webapp",
    base = file("webapp"),
    settings = buildSettings) dependsOn(core, lemmatize, tokenize, postag, chunk, parse, segment)

  lazy val core = Project(
    id = "tools-core",
    base = file("tools/core"),
    settings = buildSettings)

  lazy val lemmatize = Project(
    id = "tools-lemmatize",
    base = file("tools/lemmatize"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-lemmatize",
      licenses := Seq(
        "Academic License (for original lex files)" -> url("http://www.informatics.sussex.ac.uk/research/groups/nlp/carroll/morph.tar.gz"),
        "Apache 2.0 (for supplemental code)" -> url("http://www.opensource.org/licenses/bsd-3-clause")),
      libraryDependencies ++= Seq(clear,
        "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.5"))
  ) dependsOn(core)

  lazy val tokenize = Project(
    id = "tools-tokenize",
    base = file("tools/tokenize"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-tokenize",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(factorie))
  ) dependsOn(core)

  lazy val segment = Project(
    id = "tools-segment",
    base = file("tools/segment"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-segment",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(factorie))
  ) dependsOn(core)

  lazy val postag = Project(
    id = "tools-postag",
    base = file("tools/postag"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-postag",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        factorie,
        factoriePosModel,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value))
  ) dependsOn(tokenize)

  lazy val chunk = Project(
    id = "tools-chunk",
    base = file("tools/chunk"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-chunk",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ) dependsOn(postag)

  lazy val parse = Project(
    id = "tools-parse",
    base = file("tools/parse"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-parse",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        "org.allenai" %% "polyparser-models" % "0.1-SNAPSHOT",
        "org.allenai" %% "polyparser" % "0.1-SNAPSHOT",
        factorie,
        factorieParseModel,
        factorieWordnet,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value))
  ) dependsOn(postag)
}
