package org.allenai.nlpstack.core.remote

import dispatch.{ Http, as, url }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

trait Remote {
  def urlString: String
  def timeout = 5.minutes

  val svc = url(urlString)

  def post(string: String)(implicit executor: ExecutionContext) =
    Await.result(Http(svc << string OK as.String), timeout)
}
