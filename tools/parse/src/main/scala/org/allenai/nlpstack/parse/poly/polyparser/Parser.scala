package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.postag.defaultPostagger

import java.io.InputStream

import org.allenai.nlpstack.parse.poly.core
import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence, NexusToken }

object Parser {

  /** Loads a parser from its file.
    *
    * @param filename the JSON configuration file or model prefix
    * @return the parser initialized from the file
    */
  def loadParser(filename: String): TransitionParser = {
    TransitionParser.load(filename)
  }

  def loadParserWithCache(filename: String, parsesToCache: Iterator[PolytreeParse]): TransitionParser = {
    val fallbackParser = loadParser(filename)
    ParseCache(parsesToCache.toSeq, fallbackParser)
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

