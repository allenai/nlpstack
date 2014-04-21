package org.allenai.nlpviz

import org.allenai.aitk.tokenize._
import org.allenai.aitk.postag._
import org.allenai.aitk.chunk._
import com.typesafe.config.ConfigRenderOptions
import spray.http._
import spray.http.MediaTypes._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._
import java.awt.image.BufferedImage
import spray.httpx.SprayJsonSupport

object Tools {
  /** A class for representing a tool.
    *
    * @param  name  the name of the tool
    * @param  split  how to divide up the input text
    * @param  process  how to process each section of the input text
    * @param  visualize  conversions of the process output to a visualization
    * @param  format  conversions of the process output to a string
    */
  abstract class Tool(val name: String) {
    type Output

    def split(input: String): Seq[String]
    def process(section: String): Output
    def visualize(output: Output): Seq[BufferedImage]
    def format(output: Output): Seq[String]

    def results(section: String): (Seq[String], Seq[BufferedImage]) = {
      val processed = process(section)
      (format(processed), visualize(processed))
    }
  }

  private val tokenizer = new SimpleEnglishTokenizer()
  private val postagger = new OpenNlpPostagger()
  private val chunker = new OpenNlpChunker()

  class Tokenizer extends Tool("tokenize") {
    type Output = Seq[Token]

    override def split(input: String) = input split "\n"
    override def process(section: String) = tokenizer(section)
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(output mkString " ")
  }

  class Postagger extends Tool("postag") {
    type Output = Seq[PostaggedToken]

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer(section)
      postagger.postagTokenized(tokens)
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(output mkString " ")
  }

  class Chunker extends Tool("chunk") {
    type Output = Seq[ChunkedToken]

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer(section)
      val postags = postagger.postagTokenized(tokens)
      chunker.chunkPostagged(postags)
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(output mkString " ")
  }

/*
  class DependencyParser("Dependency Parser") {
    override def split(input: String) = input split "\n"
    override def process(section: String) =
  }
  */
}

trait ToolService extends HttpService with SprayJsonSupport {
  val tools = Seq(new Tools.Tokenizer, new Tools.Postagger, new Tools.Chunker)
  
  // format: OFF
  val toolRoute =
    pathPrefix("api" / "tools") {
      pathEnd {
        get {
          val toolNames = tools map (_.name)
          val json = toolNames.toJson
          complete(tools map (_.name))
        }
      } ~
      path(Segment) { segment =>
        tools find (_.name == segment) match {
          case Some(tool) =>
            get {
              complete(s"Post data for: ${tool.name}")
            } ~
            post {
              entity(as[String]) { body =>
                val sections: Seq[String] = tool.split(body)
                val results = sections map tool.results

                val formatted = results flatMap (_._1)
                complete(formatted mkString "\n")
              }
            }
          case None =>
            complete(StatusCodes.BadRequest -> s"Unknown tool: $segment")
        }
      }
    }
  // format: ON
}
