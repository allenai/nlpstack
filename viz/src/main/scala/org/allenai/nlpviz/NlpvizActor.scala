package org.allenai.nlpviz

import akka.actor.Actor
import spray.util.LoggingContext
import spray.routing.ExceptionHandler
import spray.http.StatusCodes

class NlpvizActor extends Actor with NlpvizService {
  
  implicit def myExceptionHandler(implicit log: LoggingContext) =
  ExceptionHandler {
    case e: Exception =>
      requestUri { uri =>
        log.error(toString, e)
        complete(StatusCodes.InternalServerError, e.getMessage)
      }
  }

  // The HttpService trait defines only one abstract member, which connects the
  // services environment to the enclosing actor or test.
  def actorRefFactory = context

  // This actor only runs our route, but you could add other things here, like
  // request stream processing or timeout handling
  def receive = runRoute(uiRoute)
}