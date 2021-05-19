package zhttp.service

import io.netty.buffer.Unpooled
import io.netty.handler.codec.http.{
  DefaultHttpResponse => JDefaultHttpResponse,
  HttpHeaderNames => JHttpHeaderNames,
  HttpVersion => JHttpVersion,
}
import zhttp.core.{JDefaultFullHttpResponse, JDefaultHttpHeaders, JHttpHeaders}
import zhttp.http.{HttpData, Response}

trait EncodeResponse {
  private val SERVER_NAME: String = "ZIO-Http"

  /**
   * Encode the [[zhttp.http.UHttpResponse]] to [io.netty.handler.codec.http.FullHttpResponse]
   */
  def encodeResponse[R, E](
    jVersion: JHttpVersion,
    res: Response.HttpResponse[R, E],
    date: String,
  ): JDefaultHttpResponse = {
    val jHttpHeaders =
      res.headers.foldLeft[JHttpHeaders](new JDefaultHttpHeaders(false)) { (jh, hh) =>
        jh.set(hh.name, hh.value)
      }
    jHttpHeaders.set(JHttpHeaderNames.SERVER, SERVER_NAME)
    jHttpHeaders.set(JHttpHeaderNames.DATE, date)
    val jStatus      = res.status.toJHttpStatus
    res.content match {
      case HttpData.CompleteData(data) =>
        val response = new JDefaultFullHttpResponse(jVersion, jStatus, Unpooled.wrappedBuffer(data.toArray), false)
        response.headers.set(JHttpHeaderNames.CONTENT_LENGTH, data.length)
        response.headers().setAll(jHttpHeaders)
        response

      case HttpData.StreamData(_) => new JDefaultHttpResponse(jVersion, jStatus, jHttpHeaders)

      case HttpData.Empty =>
        val response = new JDefaultFullHttpResponse(jVersion, jStatus, Unpooled.EMPTY_BUFFER, false)
        response.headers.set(JHttpHeaderNames.CONTENT_LENGTH, 0)
        response.headers.setAll(jHttpHeaders)
        response
    }
  }
}
