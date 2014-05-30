package org.allenai.nlpstack.nlpweb

import org.allenai.common.Resource
import org.allenai.nlpstack.nlpweb.tools._

import com.typesafe.config.ConfigRenderOptions
import org.apache.commons.codec.binary.Base64OutputStream
import spray.http._
import spray.http.MediaTypes._
import spray.routing._

import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

trait VisualizationService extends HttpService {
  val visualizers: Seq[Tool with StringFormat] = Seq(
      TokenizerTool,
      PostaggerTool,
      ChunkerTool,
      DependencyParserTool)

  // format: OFF
  val visualizationRoute =
    pathPrefix("api" / "visualize") {
      // Helpful routes for seeing what visualizations are available.
      get {
        pathEnd {
          complete((visualizers map (_.name)).sorted.mkString("\n"))
        } ~
        path(Segment)  { toolName =>
          val filtered = visualizers filter { viz =>
            viz.name equalsIgnoreCase toolName
          }

          complete((filtered map (_.name)).sorted.mkString("\n"))
        }
      } ~
      // POST routes to run the visualizations
      post {
        entity(as[String]) { body =>
          // A visualizer is specified in two parts.
          //   * the tool being visualized
          //   * the inputFormat of the POST body
          pathPrefix(Segment) { case (toolName) =>
            path("bytes") {
              visualizers find { viz =>
                (viz.name equalsIgnoreCase toolName)
              } match {
                case Some(visualizer) =>
                  val bufferedImage = visualizer.visualize(visualizer.stringFormat.read(body)).head

                  val bytes = Resource.using(new ByteArrayOutputStream()) { baos =>
                    ImageIO.write(bufferedImage, "png", baos);
                    baos.flush();
                    baos.toByteArray()
                  }

                  complete(bytes)

                case None => complete(StatusCodes.BadRequest -> s"Visualizer not found: $toolName")
              }
            } ~
            path("base64") {
              visualizers find { viz =>
                (viz.name equalsIgnoreCase toolName.toLowerCase)
              } match {
                case Some(visualizer) =>
                  val bufferedImage = visualizer.visualize(visualizer.stringFormat.read(body)).head

                  val base64 = Resource.using(new ByteArrayOutputStream()) { baos =>
                    Resource.using(new Base64OutputStream(baos)) { base64os =>
                      ImageIO.write(bufferedImage, "png", base64os)
                      baos.flush()
                      new String(baos.toByteArray())
                    }
                  }

                  complete(base64)

                case None => complete(StatusCodes.BadRequest -> s"Visualizer not found: $toolName")
              }
            }
          }
        }
      }
    }
  // format: ON
}
