package org.allenai.nlpstack

import org.allenai.nlpstack.core.segment.Segmenter

package object segment {
  def defaultSegmenter: Segmenter = new FactorieSegmenter
}
