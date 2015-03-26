import com.typesafe.sbt.SbtPgp.autoImportImpl._
import org.allenai.plugins.CoreSettingsPlugin.autoImport._
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
      publishTo := noopRepo
    )

  lazy val root = Project(
    id = "nlpstack-root",
    base = file("."),
    settings = aggregateSettings
  ).aggregate(
      tools,
      webapp,
      cli
    )

  val buildSettings =
    Revolver.settings ++
      releaseSettings ++
      Seq(
        dependencyOverrides ++= Dependencies.Overrides,
        javaOptions += s"-Dlogback.configurationFile=${file(".")}/conf/logback.xml",
        fork := true,
        organization := "org.allenai.nlpstack",
        crossScalaVersions := Seq("2.11.5"),
        scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head },
        publishMavenStyle := true,
        publishArtifact in Test := false,
        pomIncludeRepository := { _ => false },
        licenses := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
        homepage := Some(url("https://github.com/allenai/nlpstack")),
        scmInfo := Some(ScmInfo(
          url("https://github.com/allenai/nlpstack"),
          "https://github.com/allenai/nlpstack.git"
        )),
        conflictManager := ConflictManager.strict,
        libraryDependencies ++= testingLibraries,
        ReleaseKeys.publishArtifactsAction := PgpKeys.publishSigned.value,
        pomExtra :=
          <developers>
            <developer>
              <id>allenai-dev-role</id>
              <name>Allen Institute for Artificial Intelligence</name>
              <email>dev-role@allenai.org</email>
            </developer>
          </developers>,
        dependencyOverrides ++= Set(
          parserCombinators,
          "commons-codec" % "commons-codec" % "1.9",
          "joda-time" % "joda-time" % "2.7",
          "org.apache.commons" % "commons-compress" % "1.8",
          "org.allenai.common" % "common-core_2.11" % "2015.03.12-0"
        ),
        PublishTo.sonatype
      )

  lazy val tools = Project(
    id = "tools-root",
    base = file("tools"),
    settings = aggregateSettings
  ).aggregate(
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment,
      core
    )

  lazy val webapp = Project(
    id = "webapp",
    base = file("webapp"),
    settings = buildSettings
  ).enablePlugins(WebappPlugin)
    .dependsOn(
      core,
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment
    )

  lazy val cli = Project(
    id = "cli",
    base = file("cli"),
    settings = buildSettings
  ) dependsOn (
      core,
      lemmatize,
      tokenize,
      postag,
      chunk,
      parse,
      segment
    )

  lazy val core = Project(
    id = "tools-core",
    base = file("tools/core"),
    settings = buildSettings
  )

  lazy val lemmatize = Project(
    id = "tools-lemmatize",
    base = file("tools/lemmatize"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-lemmatize",
      licenses ++= Seq(
        "Academic License (for original lex files)" -> url("http://www.informatics.sussex.ac.uk/research/groups/nlp/carroll/morph.tar.gz"),
        "Apache 2.0 (for supplemental code)" -> url("http://www.opensource.org/licenses/bsd-3-clause")
      ),
      libraryDependencies ++= Seq(
        clear,
        "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.5"
      )
    )
  ) dependsOn (core)

  lazy val tokenize = Project(
    id = "tools-tokenize",
    base = file("tools/tokenize"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-tokenize",
      libraryDependencies ++= Seq(factorie, commonsIo % "test")
    )
  ) dependsOn (core)

  lazy val segment = Project(
    id = "tools-segment",
    base = file("tools/segment"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-segment",
      libraryDependencies ++= Seq(factorie, commonsIo % "test")
    )
  ) dependsOn (core)

  lazy val postag = Project(
    id = "tools-postag",
    base = file("tools/postag"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-postag",
      libraryDependencies ++= Seq(
        stanfordCoreNlp,
        factorie,
        opennlp,
        datastore,
        "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5"
      )
    )
  ) dependsOn (tokenize, core)

  lazy val chunk = Project(
    id = "tools-chunk",
    base = file("tools/chunk"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-chunk",
      libraryDependencies ++= Seq(
        opennlp,
        "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5",
        "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5"
      )
    )
  // postag-models should be a transitive dependency from the postag project, but for some
  // reason it's not brought in if it's not specified again here.
  ) dependsOn (postag, core)

  lazy val parse = Project(
    id = "tools-parse",
    base = file("tools/parse"),
    settings = buildSettings ++ Seq(
      name := "nlpstack-parse",
      libraryDependencies ++= Seq(
        factorie,
        scopt,
        datastore,
        jVerbnet
      )
    )
  ) dependsOn (postag, tokenize, core, lemmatize)
}
