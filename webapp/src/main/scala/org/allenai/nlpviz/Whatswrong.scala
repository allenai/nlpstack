package org.allenai.nlpviz

import org.allenai.common.Resource
import org.allenai.aitk.{ Reader, Writer, Format }
import org.allenai.aitk.chunk.ChunkedToken
import org.allenai.aitk.parse.graph.DependencyGraph
import org.allenai.aitk.parse.graph.DependencyNode
import org.allenai.aitk.postag.PostaggedToken
import org.allenai.aitk.srl.Frame
import org.allenai.aitk.tokenize.Token

import com.googlecode.whatswrong.NLPInstance
import com.googlecode.whatswrong.SingleSentenceRenderer
import com.googlecode.whatswrong.TokenProperty
import org.apache.commons.codec.binary.Base64OutputStream

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

/** Provides implicit readers and writers for the whatswrong format.
  *  
  * implicitly[Writer[DependencyGraph, BufferedImage]].write(dgraph)
  */
object Whatswrong {
  final val MAX_WIDTH = 2000
  final val MAX_HEIGHT = 400

  val renderer = new SingleSentenceRenderer()

  // Set colors for different edges.
  renderer.setEdgeTypeColor("amod", Color.pink)
  renderer.setEdgeTypeColor("prt", Color.pink)
  renderer.setEdgeTypeColor("advmod", Color.pink)
  renderer.setEdgeTypeColor("neg", Color.red)
  renderer.setEdgeTypeColor("det", Color.gray)
  renderer.setEdgeTypeColor("prep", Color.blue)
  renderer.setEdgeTypeColor("pobj", Color.blue)
  renderer.setEdgeTypeColor("argument", Color.blue)
  renderer.setEdgeTypeColor("relation", Color.red)
  renderer.setEdgeTypeColor("empty", Color.white)

  // Implicit tokenizers to convert NLP representations into whatswrong representations.
  implicit def tokenizeToken: WhatswrongTokenizer[Token] = new WhatswrongTokenizer[Token] {
    def tokenize(source: Token, target: com.googlecode.whatswrong.Token) = {
      target.addProperty(new TokenProperty("text", 0), source.string)
    }
  }

  implicit def tokenizePostag(implicit tokenizer: WhatswrongTokenizer[Token]): WhatswrongTokenizer[PostaggedToken] = new WhatswrongTokenizer[PostaggedToken] {
    def tokenize(source: PostaggedToken, target: com.googlecode.whatswrong.Token) {
      tokenizer.tokenize(source, target)
      target.addProperty(new TokenProperty("postag", 1), source.postag)
    }
  }
  
  implicit def tokenizeChunk(implicit tokenizer: WhatswrongTokenizer[PostaggedToken]): WhatswrongTokenizer[ChunkedToken] = new WhatswrongTokenizer[ChunkedToken] {
    def tokenize(source: ChunkedToken, target: com.googlecode.whatswrong.Token) {
      tokenizer.tokenize(source, target)
      target.addProperty(new TokenProperty("chunk", 2), source.chunk)
    }
  }

  implicit def tokenizeNode: WhatswrongTokenizer[DependencyNode] = new WhatswrongTokenizer[DependencyNode] {
    def tokenize(source: DependencyNode, target: com.googlecode.whatswrong.Token) = {
      target.addProperty(new TokenProperty("text", 0), source.string)
    }
  }

  def seq2Instance[A](seq: Seq[A])(implicit tokenizer: WhatswrongTokenizer[A]) = {
    val inst = new NLPInstance()
    for (token <- seq) {
      val tok = inst.addToken()
      tokenizer.tokenize(token, tok)
    }

    inst
  }

  def graph2Instance(graph: DependencyGraph) = {
    // get nodes
    val inst = seq2Instance[DependencyNode](graph.nodes.toSeq)

    // add edges
    for (edge <- graph.dependencies) {
      val source = edge.source.id
      val dest = edge.dest.id
      inst.addDependency(source, dest, edge.label, edge.label)
    }

    inst
  }

  def render(inst: NLPInstance) = {
    val bi = new BufferedImage(Whatswrong.MAX_WIDTH, Whatswrong.MAX_HEIGHT, BufferedImage.TYPE_INT_ARGB);
    val graphic: java.awt.Graphics2D = bi.createGraphics()
    val dimensions = renderer.synchronized {
      renderer.render(inst, graphic)
    }

    bi.getSubimage(0, 0, dimensions.width, dimensions.height)
  }

  def writeSeq2Graphic[A](implicit tokenizer: WhatswrongTokenizer[A]) = new Writer[Seq[A], BufferedImage] {
    override def write(tokens: Seq[A]): BufferedImage = {
      val inst = seq2Instance[A](tokens)
      render(inst)
    }
  }
  implicit def writeChunkSeq2Graphic[Token] = writeSeq2Graphic(tokenizeChunk)
  implicit def writePostagSeq2Graphic[PostaggedToken] = writeSeq2Graphic(tokenizePostag)
  implicit def writeTokenSeq2Graphic[ChunkedToken] = writeSeq2Graphic(tokenizeToken)
  implicit def writeGraph2Graphic = new Writer[DependencyGraph, BufferedImage] {
    val renderer = new SingleSentenceRenderer()
    override def write(graph: DependencyGraph): BufferedImage = {
      val inst = graph2Instance(graph)
      render(inst)
    }
  }

  implicit def writeFrames2Graphic = new Writer[(DependencyGraph, Frame), BufferedImage] {
    val renderer = new SingleSentenceRenderer()
    override def write(srl: (DependencyGraph, Frame)): BufferedImage = {
      val (graph, frame) = srl
      val inst = graph2Instance(graph)
      var indices = Set.empty[Int]
      inst.addSpan(frame.relation.node.id, frame.relation.node.id, frame.relation.toString, "relation")
      indices += frame.relation.node.id
      for (argument <- frame.arguments) {
        inst.addSpan(argument.node.id, argument.node.id, argument.role.label, "argument")
        indices += argument.node.id
      }
      render(inst)
    }
  }

  trait WhatswrongTokenizer[A] {
    def tokenize(source: A, dest: com.googlecode.whatswrong.Token)
  }
}
