package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core.coref.CorefResolver
import org.allenai.nlpstack.core._
import org.allenai.nlpstack.parse._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.coref._

abstract class CorefMain extends LineProcessor("coref") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  def dependencyParser: DependencyParser
  def corefResolver: CorefResolver[PostaggedToken]

  override def init(config: Config) {
    // for timing purposes
    process("I want to initialize the parser.")
  }

  override def process(line: String) = {
    val tokens = tokenizer(line)
    val postagged = postagger.postagTokenized(tokens)
    val parse = dependencyParser.dependencyGraphPostagged(postagged)
    corefResolver.resolveCoreferences((postagged, parse)).map(_.mkString).mkString("\n")
  }
}

object FactorieCorefResolverMain extends CorefMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val dependencyParser = defaultDependencyParser
  override lazy val corefResolver = new FactorieCorefResolver
}
