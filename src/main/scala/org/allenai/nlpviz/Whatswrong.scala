package org.allenai.nlpviz

import edu.knowitall.common.Resource
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.tokenize.Token

import com.googlecode.whatswrong.NLPInstance
import com.googlecode.whatswrong.SingleSentenceRenderer
import com.googlecode.whatswrong.TokenProperty
import org.apache.commons.codec.binary.Base64OutputStream

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

object Whatswrong {
  final val MAX_WIDTH = 2000
  final val MAX_HEIGHT = 400

  val renderer = new SingleSentenceRenderer()
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

  case class Base64String(string: String)

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

  implicit def writeGraphic2Base64[A](implicit writer: Writer[A, BufferedImage]) = new Writer[A, Base64String] {
    override def write(a: A) = {
      val bi = writer.write(a)
      Resource.using(new ByteArrayOutputStream()) { os =>
        Resource.using(new Base64OutputStream(os)) { b64 =>
          ImageIO.write(bi, "png", b64);
          Base64String(os.toString("UTF-8"))
        }
      }
    }
  }

  trait WhatswrongTokenizer[A] {
    def tokenize(source: A, dest: com.googlecode.whatswrong.Token)
  }

  trait Reader[A, B] {
    def read(input: A): Either[String, B]
  }
  trait Writer[A, B] {
    def write(input: A): B
  }
  trait Format[A, B] extends Reader[A, B] with Writer[A, B]

}
