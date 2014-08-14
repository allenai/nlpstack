package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.{ Format, Tokenizer, Token }
import org.allenai.nlpstack.tokenize.FactorieTokenizer.factorieFormat

import cc.factorie.app.nlp.{ Document => FactorieDocument, Token => FactorieToken, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap }
import cc.factorie.app.nlp.segment.DeterministicTokenizer

class FactorieTokenizer extends Tokenizer {
  private val tokenizer =
    new DeterministicTokenizer(tokenizeAllDashedWords = false)
  private val map = new MutableDocumentAnnotatorMap ++=
    DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  map += tokenizer
  private val pipeline = DocumentAnnotatorPipeline(
    map = map.toMap,
    prereqs = Nil,
    tokenizer.postAttrs)

  // Factorie's tokenizer crashes on unclosed XML tags. To work around this, we
  // detect unclosed tags, and replace the opening < with a space.
  private val unclosedTagRegex = "<([^>]{100})".r
  private def replaceUnclosedTag(s: String): String = {
    val replaced = unclosedTagRegex.replaceAllIn(s, m => " " + m.group(1))
    if (replaced == s)
      s
    else
      // Have to do this repeatedly for the case of "foo << barbarbarbar..."
      replaceUnclosedTag(replaced)
  }

  def tokenize(sentence: String): Seq[Token] = {
    val doc = pipeline.process(
      new FactorieDocument(
        replaceUnclosedTag(sentence)))

    factorieFormat.read(doc)
  }
}

object FactorieTokenizer {
  object factorieFormat extends Format[Seq[Token], FactorieDocument] {
    override def read(from: FactorieDocument): Seq[Token] =
      for (section <- from.sections; token <- section.tokens)
        yield Token(token.string, token.stringStart)

    override def write(from: Seq[Token]): FactorieDocument = {
      val factorieDoc = new FactorieDocument(Tokenizer.originalText(from))
      for (token <- from) {
        // creating factorie tokens modifies the factorie document
        val factorieToken = new FactorieToken(
          factorieDoc,
          token.offset,
          token.offset + token.string.length)
        factorieToken.attr += token
      }
      factorieDoc
    }
  }
}
