import org.allenai.plugins.CoreRepositories
import org.allenai.plugins.archetypes._

import Dependencies._
import sbt.Keys._
import sbt._
import spray.revolver.RevolverPlugin._
import sbtrelease.ReleasePlugin._

object NlpstackBuild extends Build {
  var noopRepo = Some(Resolver.file("Unused Repository", file("target/unusedrepo")))

  val aggregateSettings = Defaults.coreDefaultSettings ++
    releaseSettings ++
    Seq(
      publishArtifact := false,
      publishTo := noopRepo)

  lazy val root = Project(
    id = "nlpstack-root",
    base = file("."),
    settings = aggregateSettings).aggregate(
      tools,
      webapp,
      cli)

  val buildSettings =
    Revolver.settings ++ CoreRepositories.PublishTo.ai2Public ++
    releaseSettings ++
    Seq(
      organization := "org.allenai.nlpstack",
      scalaVersion := "2.10.4",
      conflictManager := ConflictManager.strict,
      resolvers += "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
      libraryDependencies ++= testingLibraries ++ loggingImplementations.map(_ % "test"),
      dependencyOverrides ++= Set(
        "com.fasterxml.jackson.core" % "jackson-core" % "2.2.3",
        "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.2",
        "com.fasterxml.jackson.core" % "jackson-annotations" % "2.2.3",
        "commons-codec" % "commons-codec" % "1.9",
        typesafeConfig,
        allenAiCommon,
        "commons-io" % "commons-io" % "2.4",
        "org.apache.commons" % "commons-compress" % "1.8"))

  lazy val tools = Project(
    id = "tools-root",
    base = file("tools"),
    settings = aggregateSettings).aggregate(
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment,
      core,
      coref)

  lazy val webapp = Project(
    id = "webapp",
    base = file("webapp"),
    settings = buildSettings).enablePlugins(WebappPlugin)
      .dependsOn(
        core,
        lemmatize,
        tokenize,
        postag,
        chunk,
        parse,
        segment,
        coref)

  lazy val cli = Project(
    id = "cli",
    base = file("cli"),
    settings = buildSettings) dependsOn(
      core,
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment,
      coref)

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
      libraryDependencies ++= Seq(factorie, commonsIo % "test"))
  ) dependsOn(core)

  lazy val segment = Project(
    id = "tools-segment",
    base = file("tools/segment"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-segment",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(factorie, commonsIo % "test"))
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
        opennlp,
        "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value))
  ) dependsOn(tokenize, core)

  lazy val chunk = Project(
    id = "tools-chunk",
    base = file("tools/chunk"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-chunk",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        opennlp,
        "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5",
        "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5"))
      // postag-models should be a transitive dependency from the postag project, but for some
      // reason it's not brought in if it's not specified again here.
  ) dependsOn(postag, core)

  lazy val parse = Project(
    id = "tools-parse",
    base = file("tools/parse"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-parse",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        ("org.allenai" %% "polyparser" % "2014.12.04-1"
          exclude("org.allenai.nlpstack", "nlpstack-postag_2.10")
          exclude("org.allenai.nlpstack", "nlpstack-tokenize_2.10")),
        factorie,
        factorieParseModel,
        factorieWordnet,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.allenai" %% "datastore" % "2014.11.24-0"))
  ) dependsOn(postag, tokenize, core)

  lazy val coref = Project(
    id = "tools-coref",
    base = file("tools/coref"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-coref",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        factorie,
        factorieCorefModel,
        factorieNerModel,
        factorieLexicon,
        factoriePhraseModel))
  ) dependsOn(core, parse)
}
