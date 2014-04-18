package org.allenai.nlpviz

import org.allenai.common.Resource
import org.allenai.nlpviz.viz._

import com.typesafe.config.ConfigRenderOptions
import org.apache.commons.codec.binary.Base64OutputStream
import spray.http._
import spray.http.MediaTypes._
import spray.routing._

import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

trait VisualizationService extends HttpService {
  val visualizers = Seq(NlptoolsDependencyParserViz)

  // format: OFF
  val visualizationRoute =
    pathPrefix("viz") {
      // Helpful routes for seeing what visualizations are available.
      get {
        pathEnd {
          complete((visualizers map (_.path)).sorted.mkString("\n"))
        } ~
        path(Segment)  { toolName =>
          val filtered = visualizers filter { viz =>
            viz.tool.names contains toolName.toLowerCase
          }

          complete((filtered map (_.path)).sorted.mkString("\n"))
        }
      } ~
      // POST routes to run the visualizations
      post {
        entity(as[String]) { body =>
          // A visualizer is specified in two parts.
          //   * the tool being visualized
          //   * the inputFormat of the POST body
          pathPrefix(Segment / Segment) { case (toolName, inputFormat) =>
            path("bytes") {
              visualizers find { viz =>
                (viz.tool.names contains toolName.toLowerCase) &&
                (viz.inputFormat equalsIgnoreCase inputFormat)
              } match {
                case Some(visualizer) =>
                  val bufferedImage = visualizer(body)

                  val bytes = Resource.using(new ByteArrayOutputStream()) { baos =>
                    ImageIO.write(bufferedImage, "png", baos);
                    baos.flush();
                    baos.toByteArray()
                  }

                  complete(bytes)

                case None => complete(StatusCodes.BadRequest -> s"Visualizer not found: $toolName/$inputFormat")
              }
            } ~
            path("base64") {
              visualizers find { viz =>
                (viz.tool.names contains toolName.toLowerCase) &&
                (viz.inputFormat equalsIgnoreCase inputFormat)
              } match {
                case Some(visualizer) =>
                  val bufferedImage = visualizer(body)

                  val base64 = Resource.using(new ByteArrayOutputStream()) { baos =>
                    Resource.using(new Base64OutputStream(baos)) { base64os =>
                      ImageIO.write(bufferedImage, "png", base64os)
                      baos.flush()
                      new String(baos.toByteArray())
                    }
                  }

                  complete(base64)

                case None => complete(StatusCodes.BadRequest -> s"Visualizer not found: $toolName/$inputFormat")
              }
            }
          }
        }
      }
    }
  // format: ON
}
