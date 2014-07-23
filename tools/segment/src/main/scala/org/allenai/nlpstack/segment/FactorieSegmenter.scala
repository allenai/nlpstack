package org.allenai.nlpstack.segment

import org.allenai.nlpstack.core.{ Segment, Segmenter }

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.{ DeterministicSentenceSegmenter, DeterministicTokenizer }

class FactorieSegmenter extends Segmenter {
  /* This is a bit unfortunate. In Factorie, you tokenize first, and then
   * segment. In nlpstack, it's the other way around. We solve the problem by
   * tokenizing twice, once here to get the sentences, and then again in
   * FactorieTokenizer. */
  private val tokenizer =
    new DeterministicTokenizer(tokenizeAllDashedWords = true)
  private val segmenter = DeterministicSentenceSegmenter
  private val map = new MutableDocumentAnnotatorMap ++=
    DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  map += tokenizer
  map += segmenter
  private val pipeline = DocumentAnnotatorPipeline(
    map = map.toMap,
    prereqs = Nil,
    segmenter.postAttrs)

  override def segment(document: String): Iterable[Segment] = {
    val doc = pipeline.process(new Document(document))

    for (sentence <- doc.sentences) yield {
      new Segment(sentence.documentString, sentence.tokens(0).stringStart)
    }
  }
}
