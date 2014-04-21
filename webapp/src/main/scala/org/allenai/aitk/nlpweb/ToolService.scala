package org.allenai.aitk.nlpweb

import org.allenai.aitk.nlpweb.tools._

import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.json.DefaultJsonProtocol._
import spray.routing.Directive.pimpApply
import spray.routing.HttpService

trait ToolService extends HttpService with SprayJsonSupport {
  val tools = Seq(
      SentenceSegmenterTool,
      LemmatizerTool,
      TokenizerTool,
      PostaggerTool,
      ChunkerTool,
      DependencyParserTool)

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
                val results = sections map tool.apply
                complete(results)
              }
            }
          case None =>
            complete(StatusCodes.BadRequest -> s"Unknown tool: $segment")
        }
      }
    }
  // format: ON
}
