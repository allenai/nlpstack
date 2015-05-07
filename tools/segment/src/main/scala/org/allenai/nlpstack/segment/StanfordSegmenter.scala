package org.allenai.nlpstack.segment

import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{ Annotation, StanfordCoreNLP }
import org.allenai.nlpstack.core.{ Segment, Segmenter }

import scala.collection.JavaConverters._

object StanfordSegmenter extends Segmenter {
  /* This is a bit unfortunate. In Stanford, you tokenize first, and then
   * segment. In nlpstack, it's the other way around. We solve the problem by
   * tokenizing twice, once here to get the sentences, and then again in
   * StanfordTokenizer. */

  private val pipeline = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    new StanfordCoreNLP(props)
  }

  override def segment(document: String): Iterable[Segment] = {
    val annotation = new Annotation(document)
    pipeline.annotate(annotation)
    annotation.get(classOf[SentencesAnnotation]).asScala.map { sentence =>
      val start = sentence.get(classOf[CoreAnnotations.CharacterOffsetBeginAnnotation])
      val end = sentence.get(classOf[CoreAnnotations.CharacterOffsetEndAnnotation])
      Segment(document.substring(start, end), start)
    }
  }
}
