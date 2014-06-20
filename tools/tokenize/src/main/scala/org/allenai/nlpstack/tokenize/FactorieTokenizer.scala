package org.allenai.nlpstack
package tokenize

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.DeterministicTokenizer

class FactorieTokenizer extends Tokenizer {
  private val tokenizer =
    new DeterministicTokenizer(tokenizeAllDashedWords = true)
  private val map = new MutableDocumentAnnotatorMap ++=
    DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  map += tokenizer
  private val pipeline = DocumentAnnotatorPipeline(
    map = map.toMap,
    prereqs = Nil,
    tokenizer.postAttrs)

  def tokenize(sentence: String): Seq[Token] = {
    val doc = pipeline.process(new Document(sentence))

    for (section <- doc.sections; token <- section.tokens)
      yield Token(token.string, token.stringStart)
  }
}

object FactorieTokenizerMain extends TokenizerMain {
  val tokenizer = new FactorieTokenizer()
}
