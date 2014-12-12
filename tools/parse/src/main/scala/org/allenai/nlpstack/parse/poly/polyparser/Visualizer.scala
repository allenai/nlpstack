package org.allenai.nlpstack.parse.poly.polyparser

import java.awt.{ Color, Paint, Dimension }
import java.util.Scanner
import javax.swing.JFrame

import edu.uci.ics.jung.algorithms.layout.{ DAGLayout, Layout }
import edu.uci.ics.jung.graph.{ DirectedSparseGraph, Graph }
import edu.uci.ics.jung.visualization.BasicVisualizationServer
import edu.uci.ics.jung.visualization.control.{ CrossoverScalingControl, ScalingControl }
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.{ Position => JungPosition }
import org.allenai.nlpstack.parse.poly.core

object Visualizer {

  /** Command-line for running an already trained parser.
    *
    * It requires one argument: the JSON configuration file for the parser.
    *
    * It will repeatedly prompt the user for a sentence to parse. Then it will parse and
    * display it (using the Jung library).
    *
    * @param args arg1 is the JSON configuration file or model file
    */
  def main(args: Array[String]) {
    // TODO: use scopt for argument processing
    val filename: String = args(0)

    val parser: TransitionParser = Parser.loadParser(filename)

    val frame: JFrame = new JFrame("Polytree Parser")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val scanner: Scanner = new Scanner(System.in)
    while (true) {
      println("Please input a sentence to parse: ")
      Parser.parseUntokenizedSentence(parser, scanner.nextLine.trim) match {
        case Some(parse) => {
          val graph = constructJungGraph(parse)
          displayJungGraph(graph, frame)
        }
        case None => println("Parse failure.")
      }
    }
  }

  /** Constructs a JUNG-style graph from a polytree parse.
    *
    * @param parse the parse tree to convert
    * @return the parse tree as a JUNG graph
    */
  def constructJungGraph(parse: PolytreeParse): Graph[VizNode, VizEdge] = {
    val graph: Graph[VizNode, VizEdge] = new DirectedSparseGraph[VizNode, VizEdge]
    val vizNodes: Vector[VizNode] = (for {
      (token, index) <- parse.tokens.zipWithIndex
    } yield VizNode(token, index)).toVector
    for (node <- vizNodes.tail) {
      graph.addVertex(node)
    }
    for {
      tokenIndex <- 1 to (parse.tokens.size - 1)
      childIndex <- parse.children(tokenIndex)
    } graph.addEdge(VizEdge(parse.arcLabelByEndNodes(Set(tokenIndex, childIndex)), tokenIndex,
      childIndex), vizNodes(tokenIndex), vizNodes(childIndex))
    graph
  }

  /** Displays a JUNG-style graph using Swing.
    *
    * @param graph the graph to display
    * @param frame the frame in which to display the graph
    */
  def displayJungGraph(graph: Graph[VizNode, VizEdge], frame: JFrame): Unit = {
    val layout: Layout[VizNode, VizEdge] = new DAGLayout(graph)
    val layoutWidth: Int = 500
    val viewAreaWidth: Int = 550
    layout.setSize(new Dimension(layoutWidth, layoutWidth));
    val vv: BasicVisualizationServer[VizNode, VizEdge] = {
      new BasicVisualizationServer[VizNode, VizEdge](layout)
    }
    vv.setPreferredSize(new Dimension(viewAreaWidth, viewAreaWidth)); //Sets the viewing area size
    vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller)
    vv.getRenderContext().setEdgeLabelTransformer(new ToStringLabeller)
    vv.getRenderer().getVertexLabelRenderer().setPosition(JungPosition.CNTR)
    vv.getRenderContext().setVertexShapeTransformer(
      new org.apache.commons.collections15.Transformer[VizNode, java.awt.Shape]() {
        def transform(label: VizNode): java.awt.Shape = {
          val longest_name = label.toString.length
          val width = longest_name * 10.0 // 12.0 is approx size of letter
          val circle = new java.awt.geom.Ellipse2D.Double(-(width / 2.0), -12.5, width, 25.0);
          circle
        }
      })
    vv.getRenderContext().setVertexFillPaintTransformer(
      new org.apache.commons.collections15.Transformer[VizNode, Paint]() {
        def transform(node: VizNode): Paint = {
          node.token.getDeterministicProperty('cpos) match {
            case 'NOUN => Color.GREEN
            case 'PRON => Color.GREEN
            case 'VERB => Color.CYAN
            case 'ADJ => Color.YELLOW
            case 'ADV => Color.YELLOW
            case _ => Color.LIGHT_GRAY
          }
        }
      })
    val scaler: ScalingControl = new CrossoverScalingControl
    scaler.scale(vv, 1 / 1.3f, vv.getCenter())
    frame.getContentPane().add(vv)
    frame.pack()
    frame.setVisible(true)
  }

}

/** Node data structure for JUNG graph visualization. */
case class VizNode(token: core.Token, sentPos: Int) {
  override def toString(): String = s"($sentPos) ${token.word.name}"
}

/** Edge data structure for JUNG graph visualization. */
case class VizEdge(arcLabel: Symbol, source: Int, dest: Int) {
  override def toString(): String = arcLabel.name
}

