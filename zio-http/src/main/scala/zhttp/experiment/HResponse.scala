package zhttp.experiment

import zhttp.http._

case class HResponse[-R, +E](
  status: Status = Status.OK,
  headers: List[Header] = Nil,
  content: HContent[R, E, Byte] = HContent.empty,
)

object HResponse {}
