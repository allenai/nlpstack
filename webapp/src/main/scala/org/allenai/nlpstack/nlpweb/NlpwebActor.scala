package org.allenai.nlpstack.nlpweb

import akka.actor.Actor
import spray.util.LoggingContext
import spray.routing._
import spray.http._

class NlpwebActor extends Actor with BasicService with VisualizationService with ToolService {

  implicit def myExceptionHandler(implicit log: LoggingContext) =
  ExceptionHandler {
    case e: Exception =>
      requestUri { uri =>
        log.error(toString, e)
        complete(StatusCodes.InternalServerError -> e.getMessage)
      }
  }

  // The HttpService trait defines only one abstract member, which connects the
  // services environment to the enclosing actor or test.
  def actorRefFactory = context

  /** Expire cached page after 60 seconds. */
  val cacheControlMaxAge = HttpHeaders.`Cache-Control`(CacheDirectives.`max-age`(0))

  // This actor only runs our route, but you could add other things here, like
  // request stream processing or timeout handling
  def receive = runRoute(basicRoute ~ visualizationRoute ~ toolRoute)
}
