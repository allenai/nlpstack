package org.allenai.aitk.nlpweb.tools

import org.allenai.aitk.nlpweb.Whatswrong
import org.allenai.aitk.Writer
import java.awt.image.BufferedImage
import org.allenai.aitk.tokenize._

 object TokenizerTool extends Tool("tokenize") with StringFormat {
    type Output = Seq[Token]

    override def info = ToolInfo(Impl.obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = Impl.tokenizer(section)
    override def visualize(output: Output) = { 
      import Whatswrong._
      Seq(
        implicitly[Writer[Output, BufferedImage]].write(output)
      )
    }
    override def stringFormat = Tokenizer.multilineStringFormat
  }