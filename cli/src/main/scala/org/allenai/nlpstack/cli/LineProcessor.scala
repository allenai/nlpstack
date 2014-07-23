package org.allenai.nlpstack.cli

import org.allenai.common.Timing

import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.http._
import spray.routing._
import spray.util.LoggingContext

import scala.concurrent.duration._
import scala.io.{ Codec, Source }
import java.io.{ File, PrintWriter }

abstract class LineProcessor(name: String) {
  val typesafeConfig = ConfigFactory.load()

  case class Config(
    server: Boolean = false,
    port: Int = typesafeConfig.getInt(s"nlpstack.tools.$name.defaultPort"),
    outputFile: Option[File] = None,
    inputFile: Option[File] = None,
    parallel: Boolean = false)

  val parser = new scopt.OptionParser[Config](name) {
    // server config
    opt[Unit]("server").text("run as a server").action { (_, c: Config) => c.copy(server = true) }
    opt[Int]("port").text("port to run the server on").action { (port: Int, c: Config) =>
      require(c.server, "--server must be set with --port"); c.copy(port = port)
    }

    // IO config
    opt[String]("input").action { (path: String, c: Config) =>
      c.copy(inputFile = Some(new File(path)))
    }.text("file to input from")
    opt[String]("output").action { (path: String, c: Config) =>
      c.copy(outputFile = Some(new File(path)))
    }.text("file to output to")

    // execution config
    opt[Unit]("parallel").action { (_, c: Config) =>
      c.copy(parallel = true)
    }.text("parallel execution")

    help("help").text("print this usage text")
  }

  def main(args: Array[String]) = {
    parser.parse(args, new Config) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def init(config: Config): Unit = {}

  def run(config: Config) {
    init(config)
    if (config.server) {
      val server = new LineProcessorServer(name, config.port, process)
      server.run()
    } else {
      runCli(config)
    }
  }

  def handle(writer: PrintWriter, line: String): Unit = {
    writer.println(process(line))
    writer.flush()
  }

  def process(line: String): String

  def runCli(config: Config) {
    val source = config.inputFile match {
      case Some(file) => Source.fromFile(file)(Codec.UTF8)
      case None => Source.fromInputStream(System.in)(Codec.UTF8)
    }

    val writer = config.outputFile match {
      case Some(file) => new PrintWriter(file, "UTF-8")
      case None => new PrintWriter(System.out)
    }

    val duration = Timing.time {
      val lines = {
        if (config.parallel) source.getLines.toIndexedSeq.par
        else source.getLines
      }
      for (line <- lines) {
        handle(writer, line)
        writer.println()
      }
    }

    System.err.println(f"${duration.toUnit(SECONDS)}%1.2f s")

    source.close()
    writer.close()
  }
}

// This is a separate class so that optional dependencies are not loaded
// unless a server instance is being create.
class LineProcessorServer(name: String, port: Int, process: String => String) {
  def run() {
    // ActorSystem to host the application in.
    implicit val system = ActorSystem(s"$name-server")

    // Create and start our service actor.
    val service = system.actorOf(Props(classOf[ToolActor], name, process), s"nlpstack-$name-actor")

    // Start a new HTTP server with our service actor as the handler.
    {
      // Timeout for starting the spray Http server (below).
      implicit val timeout = Timeout(30.seconds)

      // IO is a scala object with an apply method that returns an ActorRef.
      IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = port)
    }
  }
}

class ToolActor(name: String, process: String => String) extends HttpServiceActor {
  implicit def myExceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e: Exception =>
        requestUri { uri =>
          log.error(toString, e)
          complete(StatusCodes.InternalServerError -> e.getMessage)
        }
    }

  // format: OFF
  val route =
    path("") {
      get {
        complete("Post a line to process for: " + name)
      } ~
      post {
        entity(as[String]) { body =>
          complete(process(body))
        }
      }
    } ~
    get {
      path("info") {
        path("name") {
          complete(name)
        }
      }
    }

  // This actor only runs our route, but you could add other things here, like
  // request stream processing or timeout handling
  def receive = runRoute(route)
}
