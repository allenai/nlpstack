package org.allenai.nlpviz.tools

import org.allenai.nlpviz.Whatswrong
import org.allenai.aitk.Writer
import java.awt.image.BufferedImage
import org.allenai.aitk.postag._

 object PostaggerTool extends Tool("postag") with StringFormat {
    type Output = Seq[PostaggedToken]

    override def info = ToolInfo(Impl.obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = Impl.tokenizer(section)
      Impl.postagger.postagTokenized(tokens)
    }
    override def visualize(output: Output) = { 
      import Whatswrong._
      Seq(
        implicitly[Writer[Output, BufferedImage]].write(output)
      )
    }
    override def stringFormat = Postagger.multilineStringFormat
  }