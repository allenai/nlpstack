package org.allenai.repr.document

import org.allenai.aitk.segment._
import org.allenai.repr.sentence._

case class DocumentSentence[S <: Sentence](sentence: S, offset: Int)

trait Sentenced[S <: Sentence] {
  this: Document =>

  def sentences: Stream[DocumentSentence[S]]
}

trait Sentencer[S <: Sentence] extends Sentenced[S] {
  this: Document =>

  def constructor(text: String): S
  def sentencer: Segmenter

  override lazy val sentences: Stream[DocumentSentence[S]] =
    sentencer(text).toStream.map { segment =>
      DocumentSentence(constructor(segment.text), segment.offset)
    }
}
