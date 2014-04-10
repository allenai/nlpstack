package org.allenai.nlpviz.viz

import org.allenai.nlpviz.Tool

import java.awt.image.BufferedImage

/** A function that can visualize a particular NLP tool.
  * 
  * @param  tool  the type of NLP tool being visualized
  * @param  inputFormat  the format of the NLP tool's input
  */
abstract class Visualizer(val tool: Tool, val inputFormat: String) {
  require(inputFormat matches "[a-zA-Z]+")

  def path = s"${tool.name}/$inputFormat"

  def apply(input: String): BufferedImage
}
