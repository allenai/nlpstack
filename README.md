# AI Toolkit

This contains our basic stack of NLP tools.  It's called the AI Toolkit presently
so it's not constrained to house NLP technology.

We have general interfaces on each tool so we have a clear definition of what each
tool does and so we can change the underlying implementation of a tool.

Each tool also has a pickle format for its output.  For example, there is a
dependency string format and a chunked sentence string format.

## Project Layout

1.  tools/core: this project contains the tool interfaces and NLP representations.
2.  tools/impl: the implementations of the NLP tools
3.  webapp: a web application for running tools and visualizing serialized
    representations.

## Tools

Presently the AI Toolkit includes the following tools.

1.  Tokenizer.  Break a sentence into "word" tokens.
2.  Lemmatizer.  Associate a base form to a token or a Part-of-Speech (POS) tagged token.  The results will be more accurate if POS tags are available.
3.  Postagger.  Associate a POS tag with a token.
4.  Chunker.  Associate chunk ranges with POS-tagged tokens.
5.  Dependency Parser.  Construct dependencies between POS-tagged tokens.
6.  Segmenter.  Split a body of text into sentences.

Each tool includes:

* An API so it can be called programatically.
* A CLI application so it can be run in batch.
* A simple REST server so it can be called remotely.
