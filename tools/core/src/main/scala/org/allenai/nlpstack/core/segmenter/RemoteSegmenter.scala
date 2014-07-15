package org.allenai.nlpstack.core.segment

import org.allenai.nlpstack.core.Remote

import scala.concurrent.ExecutionContext

class RemoteSegmenter(val urlString: String)(implicit executionContext: ExecutionContext) extends Segmenter with Remote {
  def segment(sentence: String) = {
    val response = this.post(sentence)
    response.split("\\n").map(Segment.deserialize)(scala.collection.breakOut)
  }
}
