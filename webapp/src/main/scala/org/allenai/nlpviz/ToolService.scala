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
import org.allenai.aitk.lemmatize.Lemmatized
import org.allenai.aitk.segment.ChalkSentenceSegmenter
import org.allenai.aitk.lemmatize.MorphaStemmer
import org.allenai.aitk.segment.Segment
import org.allenai.aitk.parse.ClearParser
import org.allenai.aitk.parse.DependencyParser
import org.allenai.aitk.parse.graph.DependencyGraph

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

    def info: ToolInfo

    def split(input: String): Seq[String]
    def process(section: String): Output
    def visualize(output: Output): Seq[BufferedImage]
    def format(output: Output): Seq[String]

    def results(section: String): (Seq[String], Seq[BufferedImage]) = {
      val processed = process(section)
      (format(processed), visualize(processed))
    }
  }

  case class ToolInfo(example: String)
  object ToolInfo {
    implicit val toolInfoFormat = jsonFormat1(ToolInfo.apply)
  }

  private val sentenceSegmenter = new ChalkSentenceSegmenter()

  private val tokenizer = new SimpleEnglishTokenizer()
  private val lemmatizer = new MorphaStemmer()
  private val postagger = new OpenNlpPostagger()
  private val chunker = new OpenNlpChunker()

  private lazy val dependencyParser = new ClearParser()

  val obamaText = "Barack Hussein Obama II is the 44th and current President of the United States, and the first African American to hold the office. Born in Honolulu, Hawaii, Obama is a graduate of Columbia University and Harvard Law School, where he served as president of the Harvard Law Review. He was a community organizer in Chicago before earning his law degree. He worked as a civil rights attorney and taught constitutional law at the University of Chicago Law School from 1992 to 2004. He served three terms representing the 13th District in the Illinois Senate from 1997 to 2004, running unsuccessfully for the United States House of Representatives in 2000."
  val obamaSentences = sentenceSegmenter(obamaText) map (_.text) mkString "\n"

  object SentenceSegmenterTool extends Tool("segment") {
    type Output = Seq[Segment]

    override def info = ToolInfo(obamaText)

    override def split(input: String) = Seq(input)
    override def process(section: String) = sentenceSegmenter(section).toSeq
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(output mkString "\n")
  }

  object TokenizerTool extends Tool("tokenize") {
    type Output = Seq[Token]

    override def info = ToolInfo(obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = tokenizer(section)
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(Tokenizer.multilineStringFormat.write(output))
  }

  object LemmatizerTool extends Tool("lemmatize") {
    type Output = Seq[Lemmatized[Token]]

    override def info = ToolInfo(obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer.tokenize(section)
      val postagged = postagger.postagTokenized(tokens)
      postagged map lemmatizer.lemmatizePostaggedToken
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(output mkString " ")
  }

  object PostaggerTool extends Tool("postag") {
    type Output = Seq[PostaggedToken]

    override def info = ToolInfo(obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer(section)
      postagger.postagTokenized(tokens)
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(Postagger.multilineStringFormat.write(output))
  }

  object ChunkerTool extends Tool("chunk") {
    type Output = Seq[ChunkedToken]

    override def info = ToolInfo(obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer(section)
      val postags = postagger.postagTokenized(tokens)
      chunker.chunkPostagged(postags)
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(Chunker.multilineStringFormat.write(output))
  }

  object DependencyParserTool extends Tool("dependencies") {
    type Output = DependencyGraph

    override def info = ToolInfo(obamaSentences)

    override def split(input: String) = input split "\n"
    override def process(section: String) = {
      val tokens = tokenizer(section)
      val postags = postagger.postagTokenized(tokens)
      dependencyParser.dependencyGraphPostagged(postags)
    }
    override def visualize(output: Output) = Seq.empty
    override def format(output: Output) = Seq(DependencyGraph.multilineStringFormat.write(output))
  }
}

trait ToolService extends HttpService with SprayJsonSupport {
  val tools = Seq(
      Tools.SentenceSegmenterTool,
      Tools.LemmatizerTool,
      Tools.TokenizerTool,
      Tools.PostaggerTool,
      Tools.ChunkerTool,
      Tools.DependencyParserTool)

  // format: OFF
  val toolRoute =
    pathPrefix("api" / "tools") {
      pathEnd {
        get {
          val toolNames = tools map (_.name)
          complete(tools map (_.name))
        }
      } ~
      path(Segment) { segment =>
        tools find (_.name == segment) match {
          case Some(tool) =>
            get {
              complete(tool.info)
            } ~
            post {
              entity(as[String]) { body =>
                val sections: Seq[String] = tool.split(body)
                val results = sections map tool.results

                val formatted = results flatMap (_._1)
                complete(formatted)
              }
            }
          case None =>
            complete(StatusCodes.BadRequest -> s"Unknown tool: $segment")
        }
      }
    }
  // format: ON
}
