package org.allenai.nlpstack.postag

import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import org.allenai.nlpstack.tokenize.Token

class FactoriePostagger extends Postagger {
  // We need to add a tokenizer manually because the default one that factorie
  // uses splits hyphenated words, and we don't like that.
  // Not sure why Factorie uses a tokenizer for this at all.
  private val tokenizer =
    new DeterministicTokenizer(tokenizeAllDashedWords = true)
  private val tagger = OntonotesForwardPosTagger
  private val map = new MutableDocumentAnnotatorMap ++=
    DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  map += tokenizer
  map += tagger
  private val pipeline = DocumentAnnotatorPipeline(
    map = map.toMap,
    prereqs = Nil,
    tagger.postAttrs)

  override def postagTokenized(tokens: Seq[Token]): Seq[PostaggedToken] = {
    // translate the tokens into a Factorie document
    val str = new StringBuilder
    for (token <- tokens) {
      if (str.length < token.offset)
        str.append(" " * (token.offset - str.length))
      str.replace(token.offset, token.offset + token.string.length, token.string)
    }
    val factorieDoc = new Document(str.mkString)
    val section = new BasicSection(factorieDoc, 0, str.length)
    val factorieTokens = tokens.map(
      t => new cc.factorie.app.nlp.Token(t.offset, t.offset + t.string.length))
    section ++= factorieTokens

    val doc = pipeline.process(factorieDoc)

    for (section <- doc.sections; token <- section.tokens)
      yield PostaggedToken(
      tagger.tokenAnnotationString(token),
      token.string,
      token.stringStart)
  }
}
