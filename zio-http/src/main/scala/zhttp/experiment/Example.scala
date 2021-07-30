package zhttp.experiment

import zhttp.http._
import zio.Chunk

object Example {
  val app      = Http.collect[AnyRequest] { case req => HResponse(content = req.content) }
  val greet    = Http.collect[AnyRequest] { case _ =>
    HResponse(Status.OK, Nil, HContent.from(Chunk.fromArray("Greetings".getBytes())))
  }
  val notFound = Http.collect[AnyRequest] { case _ => HResponse(Status.NOT_FOUND, Nil, HContent.empty) }
}
