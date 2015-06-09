package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.postag.defaultPostagger

import java.io.InputStream

import org.allenai.nlpstack.parse.poly.core
import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence, NexusToken }

object Parser {

  //def createDefaultParser(parserConfig: ParserConfiguration): TransitionParser = {
  //  new RerankingTransitionParser(parserConfig)
  //}

  /** Loads a parser from its file.
    *
    * @param filename the JSON configuration file or model prefix
    * @return the parser initialized from the file
    */
  def loadParser(filename: String): TransitionParser = {
    TransitionParser.load(filename)
  }

  /** Loads a parser from an InputStream of a models file
    * @param inputStream stream of models config file
    * @return the parser initialized from the input stream
    */
  def loadParser(inputStream: InputStream): TransitionParser = {
    TransitionParser.loadFromStream(inputStream)
  }

  private val tokenizer = defaultTokenizer
  private val postagger = defaultPostagger

  /** Tokenizes, tags, and parses an untokenized sentence.
    *
    * @param parser the parser to use for parsing the sentence
    * @param text the untokenized sentence
    * @return a parse for the argument sentence
    */
  def parseUntokenizedSentence(parser: TransitionParser, text: String): Option[BankerParse] = {
    parser.parse(Sentence(tokenizeSentence(text).toIndexedSeq)) map { parse => parse.asBankerParse }
  }

  def parseWithConstraints(
    parser: TransitionParser,
    sentence: Sentence,
    constraints: Set[TransitionConstraint]
  ): Option[BankerParse] = {

    println(s"parseWithConstraints: $constraints")
    parser.parse(sentence, constraints) map { parse => parse.asBankerParse }
  }

  /** Tokenizes (and tags) an untokenized sentence.
    *
    * @param text the untokenized sentence
    * @return a sequence of tokens
    */
  def tokenizeSentence(text: String): Seq[core.Token] = {
    val postagged: Seq[PostaggedToken] = postagger.postag(tokenizer)(text)
    NexusToken +: (postagged map {
      case tok =>
        core.Token(
          word = Symbol(tok.string),
          Map(
            'autoPos ->
              Set(Symbol(tok.postag)),
            'autoCpos ->
              Set(Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(tok.postag, tok.postag)))
          )
        )
    })
  }
}

