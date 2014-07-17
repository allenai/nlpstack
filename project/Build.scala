import org.allenai.sbt.deploy._
import org.allenai.sbt.travispublisher._

import Dependencies._
import sbt.Keys._
import sbt._
import spray.revolver.RevolverPlugin._

import scala.collection.JavaConverters._
import java.io.{FileInputStream, IOException}
import java.util.Properties

object NlpstackBuild extends Build {
  var noopRepo = Some(Resolver.file("Unused Repository", file("target/unusedrepo")))

  val aggregateSettings = Defaults.coreDefaultSettings ++
      Seq(
        publishArtifact := false,
        publishTo := noopRepo)

  def readProperties(file: File) : Option[Properties] = {
    val properties = new Properties()
    try {
      // read the file
      val input = new FileInputStream(file)
      try {
        properties.load(input)
      } finally {
        input.close()
      }
      Some(properties)
    } catch {
      case e: IOException => {
        None
      }
    }
  }

  def propertiesToMap(properties: Properties) : Map[String, String] = {
    val pnames = properties.stringPropertyNames.asScala
    def makePropTuple(pname: String) : Tuple2[String, String] = {
      (pname, properties.getProperty(pname))
    }
    pnames.map(makePropTuple(_)).toMap
  }

  def getCredentialTuple(map: Map[String, String]) : Option[Tuple2[String, String]] = {
    for(
      user <- map.get("NEXUS_USER");
      password <- map.get("NEXUS_PASS")
    ) yield (user, password)
  }

  def credentialsFromFile(file: File) : Option[Credentials] = {
    for(
      properties <- readProperties(file);
      tuple <- getCredentialTuple(propertiesToMap(properties))
    ) yield Credentials(
      "Sonatype Nexus Repository Manager",
      "utility.allenai.org",
      tuple._1,
      tuple._2
    )
  }

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
        "commons-io" % "commons-io" % "2.4"),
      credentials ++= credentialsFromFile(new File("./creds/nexus.properties")).toSeq)

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
      core)

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
  ).enablePlugins(TravisPublisherPlugin) dependsOn(tokenize)

  lazy val chunk = Project(
    id = "tools-chunk",
    base = file("tools/chunk"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-chunk",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(postag)

  lazy val parse = Project(
    id = "tools-parse",
    base = file("tools/parse"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-parse",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        "org.allenai" %% "polyparser-models" % "0.1-SNAPSHOT",
        ("org.allenai" %% "polyparser" % "0.2"
          exclude("org.allenai.nlpstack", "nlpstack-postag_2.10")
          exclude("org.allenai.nlpstack", "nlpstack-tokenize_2.10")),
        factorie,
        factorieParseModel,
        factorieWordnet,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value))
  ).enablePlugins(TravisPublisherPlugin) dependsOn(postag, tokenize)
}
