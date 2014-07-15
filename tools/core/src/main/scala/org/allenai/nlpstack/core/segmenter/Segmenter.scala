package org.allenai.nlpstack.core.segment

import org.allenai.common.immutable.Interval

import spray.json.DefaultJsonProtocol._

/** A sentencer breaks text into sentences.
  */
abstract class Segmenter {
  def apply(document: String) = segment(document)

  def segmentTexts(document: String) = {
    this.segment(document).map(_.text)
  }

  def segment(document: String): Iterable[Segment]
}

case class Segment(text: String, offset: Int) {
  override def toString = serialize

  def interval = Interval.open(offset, offset + text.length)
  def length = text.length

  def serialize = text + "@" + offset
}

object Segment {
  private[this] val segmentRegex = """(.+)@(\d+)""".r
  def deserialize(pickled: String): Segment = {
    pickled match {
      case segmentRegex(string, offset) => new Segment(string, offset.toInt)
      case s => throw new MatchError("Could not deserialize: " + s)
    }
  }

  implicit val segmentJsonFormat = jsonFormat2(Segment.apply)
}
