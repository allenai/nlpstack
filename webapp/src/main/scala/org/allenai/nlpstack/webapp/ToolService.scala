package org.allenai.nlpstack.webapp

import org.allenai.nlpstack.webapp.tools._

import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.json.DefaultJsonProtocol._ // IntelliJ thinks this is unused, but it's not.
import spray.routing.Directive.pimpApply
import spray.routing.HttpService

trait ToolService extends HttpService with SprayJsonSupport {
  val tools = Seq(
    SentenceSegmenterTool,
    LemmatizerTool,
    TokenizerTool,
    PostaggerTool,
    ChunkerTool,
    DependencyParserTool
  )

  // format: OFF
  val toolRoute =
    pathPrefix("api" / "tools") {
      // List available tools in JSON.
      pathEnd {
        get {
          val toolNames = tools map (_.name)
          complete(tools map (_.name))
        }
      } ~
      path(Segment) { segment =>
        tools find (_.name == segment) match {
          case Some(tool) =>
            // Give info about this tool.
            get {
              complete(tool.info)
            } ~
            // Process text with this tool.
            post {
              entity(as[String]) { body =>
                val sections: Seq[String] = tool.split(body)
                val results = sections map tool.apply
                complete(results)
              }
            }
          case None =>
            // Tool not found.
            complete(StatusCodes.BadRequest -> s"Unknown tool: $segment")
        }
      }
    }
  // format: ON
}
