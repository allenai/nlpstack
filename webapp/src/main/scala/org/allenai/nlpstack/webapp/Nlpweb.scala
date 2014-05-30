package org.allenai.nlpstack.webapp

import akka.actor.ActorSystem
import akka.actor.Props
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import spray.can.Http

import scala.concurrent.duration.DurationInt

object Nlpweb {
  lazy val config = ConfigFactory.load()
  val name = "webapp"

  def main(args: Array[String]): Unit = {
    // ActorSystem to host the application in.
    implicit val system = ActorSystem("webapp")

    // Create and start our service actor.
    val service = system.actorOf(Props[NlpwebActor], "webapp-actor")

    // Start a new HTTP server with our service actor as the handler.
    {
      // Timeout for starting the spray Http server (below).
      implicit val timeout = Timeout(30.seconds)

      // IO is a scala object with an apply method that returns an ActorRef.
      IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = config.getInt("nlpstack.webapp.port"))
    }
  }
}
