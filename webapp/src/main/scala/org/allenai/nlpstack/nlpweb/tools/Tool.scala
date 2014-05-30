package org.allenai.nlpstack.nlpweb.tools

import org.allenai.nlpstack.Format
import org.allenai.common.Resource
import org.apache.commons.codec.binary.Base64OutputStream
import org.apache.commons.io.output.ByteArrayOutputStream

import spray.json.DefaultJsonProtocol.StringJsonFormat
import spray.json.DefaultJsonProtocol.jsonFormat1
import spray.json.DefaultJsonProtocol.jsonFormat2
import spray.json.DefaultJsonProtocol.seqFormat

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/**
 * A class for representing a tool.
 *
 * @param  name  the name of the tool
 * @param  split  how to divide up the input text
 * @param  process  how to process each section of the input text
 * @param  visualize  conversions of the process output to a visualization
 * @param  format  conversions of the process output to a string
 */
abstract class Tool(val name: String) {
  type Output

  /** This information is presented on /tools/name. */
  def info: ToolInfo

  /**
   * The input to all tools is a single text box.  It may be split up
   * as the tool sees fit.  For example, a sentence segmenter may not
   * want to split the text, but a tokenizer might want to split the
   * input by newline.
   */
  def split(input: String): Seq[String]
  def process(section: String): Output
  def visualize(output: Output): Seq[BufferedImage]
  def format(output: Output): Seq[String]

  /** Process, visualize, format, and then bundle the results. */
  def apply(section: String): ToolResponse = {
    val processed = process(section)

    val visualizations = visualize(processed)
    val base64Visualizations = visualizations map { bufferedImage =>
      Resource.using(new ByteArrayOutputStream()) { baos =>
        Resource.using(new Base64OutputStream(baos)) { base64os =>
          ImageIO.write(bufferedImage, "png", base64os)
          baos.flush()
          new String(baos.toByteArray())
        }
      }
    }

    ToolResponse(format(processed), base64Visualizations)
  }
}

trait StringFormat { this: Tool =>
  def stringFormat: Format[Output, String]
  def format(output: Output): Seq[String] = Seq(stringFormat.write(output))
}

case class ToolInfo(impl: String, example: String)
object ToolInfo {
  implicit val toolInfoFormat = jsonFormat2(ToolInfo.apply)
}

case class ToolResponse(texts: Seq[String], base64Images: Seq[String])
object ToolResponse {
  implicit val toolResponseJsonFormat = jsonFormat2(ToolResponse.apply)
}
  