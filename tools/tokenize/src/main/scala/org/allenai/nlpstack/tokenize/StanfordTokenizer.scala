package org.allenai.nlpstack.tokenize

import java.io.StringReader
import scala.collection.{ mutable, JavaConverters }

import edu.stanford.nlp.process.PTBTokenizer
import org.allenai.nlpstack.core.{ Token, Tokenizer }

object StanfordTokenizer extends Tokenizer {
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
