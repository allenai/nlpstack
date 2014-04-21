package org.allenai.nlpviz.tools

import org.allenai.aitk.chunk.ChunkedToken
import org.allenai.nlpviz.Whatswrong
import org.allenai.aitk.Writer
import org.allenai.aitk.chunk.Chunker
import java.awt.image.BufferedImage

  object ChunkerTool extends Tool("chunk") with StringFormat {
    type Output = Seq[ChunkedToken]

    override def info = ToolInfo(Impl.obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = Impl.tokenizer(section)
      val postags = Impl.postagger.postagTokenized(tokens)
      Impl.chunker.chunkPostagged(postags)
    }
    override def visualize(output: Output) = { 
      import Whatswrong._
      Seq(
        implicitly[Writer[Output, BufferedImage]].write(output)
      )
    }
    override def stringFormat = Chunker.stringFormat
  }