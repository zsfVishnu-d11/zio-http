package zhttp.service

import io.netty.buffer.Unpooled
import io.netty.handler.codec.http.{
  DefaultHttpResponse => JDefaultHttpResponse,
  HttpHeaderNames => JHttpHeaderNames,
  HttpVersion => JHttpVersion,
}
import zhttp.core.{JDefaultFullHttpResponse, JDefaultHttpHeaders, JHttpHeaders}
import zhttp.http.{HttpData, Response}

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

trait EncodeResponse {
  private val SERVER_NAME: String = "ZIO-Http"

  /**
   * Encode the [[zhttp.http.UHttpResponse]] to [io.netty.handler.codec.http.FullHttpResponse]
   */
  def encodeResponse[R, E](jVersion: JHttpVersion, res: Response.HttpResponse[R, E]): JDefaultHttpResponse = {
    val jHttpHeaders =
      res.headers.foldLeft[JHttpHeaders](new JDefaultHttpHeaders()) { (jh, hh) =>
        jh.set(hh.name, hh.value)
      }
    jHttpHeaders.set(JHttpHeaderNames.SERVER, SERVER_NAME)
    jHttpHeaders.set(JHttpHeaderNames.DATE, s"${DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now)}")
    val jStatus      = res.status.toJHttpStatus
    res.content match {
      case HttpData.CompleteData(data) =>
        val response = new JDefaultFullHttpResponse(jVersion, jStatus, Unpooled.wrappedBuffer(data.toArray), false)
        jHttpHeaders.set(JHttpHeaderNames.CONTENT_LENGTH, data.length)
        response.headers().setAll(jHttpHeaders)
        response

      case HttpData.StreamData(_) => new JDefaultHttpResponse(jVersion, jStatus, jHttpHeaders)

      case HttpData.Empty =>
        jHttpHeaders.set(JHttpHeaderNames.CONTENT_LENGTH, 0)
        val response = new JDefaultFullHttpResponse(jVersion, jStatus, Unpooled.EMPTY_BUFFER, false)
        response.headers.setAll(jHttpHeaders)
        response
    }
  }
}
