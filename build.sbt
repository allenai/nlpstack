import Dependencies._

import com.typesafe.sbt.SbtPgp.autoImportImpl._

import sbtrelease._
import sbtrelease.ReleaseStateTransformations._

val sonatypeUrl = "https://oss.sonatype.org"
val sonatype = Some("Sonatype Releases" at s"${sonatypeUrl}/service/local/staging/deploy/maven2")

val noPublishSettings = Seq(
  // Don't publish a jar for the root project.
  publishArtifact := false,
  publishTo := Some("dummy" at "nowhere"),
  publish := { },
  publishLocal := { }
)

val buildSettings = Seq(
  javaOptions += s"-Dlogback.configurationFile=${file(".")}/conf/logback.xml",
  fork := true,
  organization := "org.allenai.nlpstack",
  crossScalaVersions := Seq(defaultScalaVersion),
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
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
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
    "com.typesafe" % "config" % "1.3.0",
    "io.spray" % "spray-json_2.11" % "1.3.2",
    "joda-time" % "joda-time" % "2.7",
    "org.allenai.common" % "common-core_2.11" % "1.4.6",
    "org.apache.commons" % "commons-compress" % "1.8",
    "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.3",
    "org.slf4j" % "log4j-over-slf4j" % Logging.slf4jVersion,
    "org.parboiled" % "parboiled-core" % "1.1.7"
   ),
   publishTo := sonatype
)

lazy val root = Project(
  id = "nlpstack-root",
  base = file(".")
).settings(buildSettings ++ noPublishSettings)
 .aggregate(
   tools,
   webapp,
   cli
 )

lazy val tools = Project(
  id = "tools-root",
  base = file("tools")
).settings(buildSettings ++ noPublishSettings)
 .aggregate(
   headword,
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
).dependsOn(
  core,
  headword,
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
  settings = buildSettings ++ Seq(
    libraryDependencies ++= Seq(
      allenAiCommon,
      allenAiTestkit
    )
  )
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
      allenAiTestkit,
      clear,
      "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.5"
    )
  )  
).dependsOn(core)

lazy val tokenize = Project(
  id = "tools-tokenize",
  base = file("tools/tokenize"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-tokenize",
    libraryDependencies ++= Seq(
      allenAiTestkit,
      commonsIo % "test",
      factorie,
      stanfordCoreNlp)
  )
).dependsOn(core)

lazy val segment = Project(
  id = "tools-segment",
  base = file("tools/segment"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-segment",
    libraryDependencies ++= Seq(
      allenAiTestkit,
      factorie,
      stanfordCoreNlp,
      commonsIo % "test"
    )
  )
).dependsOn(core)

lazy val postag = Project(
  id = "tools-postag",
  base = file("tools/postag"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-postag",
    libraryDependencies ++= Seq(
      allenAiTestkit,
      stanfordCoreNlp,
      factorie,
      opennlp,
      datastore,
      "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5"
    )
  )
).dependsOn(tokenize, core)

lazy val chunk = Project(
  id = "tools-chunk",
  base = file("tools/chunk"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-chunk",
    libraryDependencies ++= Seq(
      allenAiTestkit,
      opennlp,
      "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5",
      "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5"
    )
  )
  // postag-models should be a transitive dependency from the postag project, but for some
  // reason it's not brought in if it's not specified again here.
 ) dependsOn (postag, core)

lazy val headword = Project(
  id = "tools-headword",
  base = file("tools/headword"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-headword",
    libraryDependencies ++= Seq(allenAiTestkit, jwiWordnet)
  )
).dependsOn(postag, tokenize)

lazy val parse = Project(
  id = "tools-parse",
  base = file("tools/parse"),
  settings = buildSettings ++ Seq(
    name := "nlpstack-parse",
    libraryDependencies ++= Seq(
      allenAiTestkit,
      factorie,
      scopt,
      reming,
      datastore,
      jVerbnet
    )
  )
).dependsOn(postag, tokenize, core, lemmatize)
