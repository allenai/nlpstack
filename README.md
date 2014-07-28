# NLP Stack

This contains our basic stack of NLP tools. You can play with them [here](http://nlpstack.dev.allenai.org:8062/tools.html).

We have general interfaces on each tool so we have a clear definition of the
inputs and outputs of each tool and so we can change the underlying
implementation of a tool.

Each tool also has a serialization format for its output.  For example, there
is a dependency string format and a chunked sentence string format.

## Getting started

1.  NLPStack uses Factorie for a lot of things. Unfortunately, Factorie is not on Maven Central. To make it available to your project, you have to add it to your resolvers with this line:

    ```scala
    "IESL Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"
    ```
    The complete list of resolvers might look like this:
    ```scala
    resolvers ++= Seq(
      "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots",
      "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
      "IESL Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public",
      Resolver.sonatypeRepo("snapshots"))
    ```
2.  Add NLPStack to your dependencies. NLPStack comes as a collection of multiple tools (see below). To declare dependencies, you can use this code in your Build.scala file:

    ```scala
    libraryDependencies += "org.allenai.nlpstack" %% "nlpstack-core" % "0.x"

    libraryDependencies += "org.allenai.nlpstack" %% "nlpstack-parse" % "0.x"

    libraryDependencies += "org.allenai.nlpstack" %% "nlpstack-postag" % "0.x"
    ```
    As an option, you can define a function for the various nlpstack components, and use them like this:
    ```scala
    def nlpstackModule(id: String) = "org.allenai.nlpstack" %% s"nlpstack-${id}" % "0.x"

    libraryDependencies += nlpstackModule("parse")
    ```
    To find out what the latest version is, go [here](http://utility.allenai.org:8081/nexus/content/repositories/releases/org/allenai/nlpstack/nlpstack-core_2.10/), and use the latest one there.
    
3.  Start using NLPStack. Here is a quick code snippet that parses a sentence:

    ```scala
    import org.allenai.nlpstack.tokenize.defaultTokenizer
    import org.allenai.nlpstack.postag.defaultPostagger
    import org.allenai.nlpstack.parse.defaultDependencyParser

    /* ... */

    val tokens = defaultTokenizer.tokenize(
      "I was wondering why the ball kept getting bigger and bigger, and then it hit me.")
    val postaggedTokens = defaultPostagger.postagTokenized(tokens)
    val dependencyGraph = defaultDependencyParser.dependencyGraphPostagged(postaggedTokens)
    ```

## Folder Layout

1.  tools: this project contains the main Nlpstack code.
2.  webapp: a web application for running tools and visualizing serialized
    representations.

## Tools in the Kit

Presently the AI Toolkit includes the following tools.

1.  **Tokenizer**.  Break a sentence into "word" tokens.
2.  **Lemmatizer**.  Associate a base form to a token or a Part-of-Speech (POS) tagged token.  The results will be more accurate if POS tags are available.
3.  **Postagger**.  Associate a POS tag with a token.
4.  **Chunker**.  Associate chunk ranges with POS-tagged tokens.
5.  **Dependency Parser**.  Construct dependencies between POS-tagged tokens.
6.  **Segmenter**.  Split a body of text into sentences.

Each tool includes:

* An API so it can be called programatically.
* A CLI application so it can be run in batch.
* A simple REST server so it can be called remotely.

## Tool Subprojects

Nlpstack is split up into multiple subprojects to minimize the number of
dependencies needed to install components. The source for each of these is in
`tools/${projectName}`.

`tools-core`: This contains all of the APIs needed for interoperating with Nlpstack, but none of the implementations.
`tools-segment`: Implementation of the segmenter. Depends on `core`.
`tools-lemmatize`: Implementation of the lemmatizer. Depends on `core`.
`tools-tokenize`: Implementation of the tokenizer. Depends on `core`.
`tools-postag`: Implementation of the POS tagger. Depends on `tokenize`.
`tools-chunk`: Implementation of the sentence chunker. Depends on `postag`.
`tools-parse`: Implementation of the dependency parser. Depends on `postag`.

These each produce a single artifact, named `nlptools-${projectName}`.
A client should depend on every implementation they will be using, as well as `nlpstack-core`.

These all use the group `org.allenai.nlpstack`.

So, if you wanted to use the tokenizer, you should have the dependencies (in sbt):

```
"org.allenai.nlpstack" %% "nlpstack-core" % "2014.6.23-1-SNAPSHOT"
"org.allenai.nlpstack" %% "nlpstack-tokenize" % "2014.6.23-1-SNAPSHOT"
```

The current version is in [version.sbt](version.sbt).

### Parsing API Details

The example in "Getting Started" shows how to generate a
[dependency graph](https://github.com/allenai/nlpstack/blob/master/tools/core/src/main/scala/org/allenai/nlpstack/parse/graph/DependencyGraph.scala)
from a sentence. The graph object itself contains [dependency nodes](https://github.com/allenai/nlpstack/blob/master/tools/core/src/main/scala/org/allenai/nlpstack/parse/graph/DependencyNode.scala)
with integer IDs. These IDs can be used to index the original tokens given to the parser.

If you want to have lemmatized token information, you'll want to run the tokens through a lemmatizer:
```
import org.allenai.nlpstack.lemmatize.MorphaStemmer

val lemmatizer = new MorphaStemmer()
val lemmatizedTokens = postaggedTokens map { lemmatizer.lemmatizePostaggedToken }
```

Once you have lemmatized tokens, you can build a new dependency graph with token information contained in the nodes:
```
val dependencyGraphWithTokenInfo = dependencyGraph.tokenized(lemmatizedTokens)
```
