package org.allenai.nlpviz.viz

import org.allenai.aitk.Writer
import org.allenai.nlpviz.Dependencies
import org.allenai.nlpviz.Whatswrong._

import org.allenai.aitk.parse.graph.DependencyGraph

import java.awt.image.BufferedImage

object NlptoolsDependencyParserViz extends Visualizer(Dependencies, "nlptools") {
  override def apply(input: String) = {
    val graph = DependencyGraph.multilineStringFormat.read(input)
    implicitly[Writer[DependencyGraph, BufferedImage]].write(graph)
  }
}
