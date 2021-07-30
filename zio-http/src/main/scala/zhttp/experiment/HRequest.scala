package zhttp.experiment

import zhttp.http._

case class HRequest[-R, +E](
  endpoint: Endpoint,
  headers: List[Header] = List.empty,
  content: HContent[R, E, Byte],
)

object HRequest {}
