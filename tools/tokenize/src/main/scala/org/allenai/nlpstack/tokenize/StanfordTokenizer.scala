package org.allenai.nlpstack.tokenize

import java.io.StringReader
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.collection.{ mutable, JavaConverters }

import edu.stanford.nlp.process.PTBTokenizer
import org.allenai.nlpstack.core.{ Token, Tokenizer }

object StanfordTokenizer extends Tokenizer {
  // redirect stanford's logging
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  var averageTokenLength = 6 // low estimates are better
  private val tokenizerFactory = PTBTokenizer.factory()
  tokenizerFactory.setOptions("untokenizable=allKeep")

  def tokenize(sentence: String) = {
    val reader = new StringReader(sentence)
    val tokenizer = tokenizerFactory.getTokenizer(reader)
    val result = new mutable.ArrayBuffer[Token](sentence.length / averageTokenLength)

    while (tokenizer.hasNext) {
      val token = tokenizer.next()
      result += Token(sentence.substring(token.beginPosition(), token.endPosition()), token.beginPosition())
    }
    result
  }
}
