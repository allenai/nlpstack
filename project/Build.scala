import org.allenai.sbt.deploy._
import org.allenai.sbt.travispublisher._

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
    Revolver.settings ++
    Publish.settings ++
    releaseSettings ++
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
      libraryDependencies ++= testingLibraries ++ loggingImplementations.map(_ % "test"),
      dependencyOverrides ++= Set(
        "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "commons-io" % "commons-io" % "2.4"))

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
    settings = buildSettings).enablePlugins(DeployPlugin, TravisPublisherPlugin) dependsOn(
      core,
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment)

  lazy val cli = Project(
    id = "cli",
    base = file("cli"),
    settings = buildSettings).enablePlugins(DeployPlugin, TravisPublisherPlugin) dependsOn(
      core,
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment)

  lazy val core = Project(
    id = "tools-core",
    base = file("tools/core"),
    settings = buildSettings).enablePlugins(TravisPublisherPlugin)

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
  ).enablePlugins(TravisPublisherPlugin) dependsOn(core)

  lazy val tokenize = Project(
    id = "tools-tokenize",
    base = file("tools/tokenize"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-tokenize",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(factorie))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(core)

  lazy val segment = Project(
    id = "tools-segment",
    base = file("tools/segment"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-segment",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(factorie))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(core)

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
  ).enablePlugins(TravisPublisherPlugin) dependsOn(tokenize, core)

  lazy val chunk = Project(
    id = "tools-chunk",
    base = file("tools/chunk"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-chunk",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(postag, core)

  lazy val parse = Project(
    id = "tools-parse",
    base = file("tools/parse"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-parse",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        "org.allenai" %% "polyparser-models" % "0.3-SNAPSHOT",
        ("org.allenai" %% "polyparser" % "2014.07.23-1"
          exclude("org.allenai.nlpstack", "nlpstack-postag_2.10")
          exclude("org.allenai.nlpstack", "nlpstack-tokenize_2.10")),
        factorie,
        factorieParseModel,
        factorieWordnet,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(postag, tokenize, core)

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
  ).enablePlugins(TravisPublisherPlugin) dependsOn(core, parse)
}
