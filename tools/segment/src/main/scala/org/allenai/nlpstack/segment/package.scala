package org.allenai.nlpstack

package object segment {
  def defaultSegmenter: Segmenter = new FactorieSegmenter
}
