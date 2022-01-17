package zhttp.service

import io.netty.buffer.Unpooled
import io.netty.handler.codec.http.{DefaultFullHttpRequest, FullHttpRequest, HttpHeaderNames, HttpVersion}
import zhttp.http.HTTP_CHARSET
trait EncodeClientParams {
  /**
   * Converts client params to JFullHttpRequest
   */
  def encodeClientParams(jVersion: HttpVersion, req: Client.ClientParams): FullHttpRequest = {
    val method          = req.method.asHttpMethod
    val reqHeaders = req.getHeaders
    val url             = req.url
    val uri             = url.asString
    val content         = req.getBodyAsString match {
      case Some(text) => Unpooled.copiedBuffer(text, HTTP_CHARSET)
      case None       => Unpooled.EMPTY_BUFFER
    }

    val reqHeadersWithHost = url.host match {
      case Some(value) => reqHeaders.addHeader(HttpHeaderNames.HOST, value)
      case None        => reqHeaders
    }

    val headers = reqHeadersWithHost.encode

    val writerIndex = content.writerIndex()
    if (writerIndex != 0) {
      headers.set(HttpHeaderNames.CONTENT_LENGTH, writerIndex.toString())
    }
    // TODO: we should also add a default user-agent req header as some APIs might reject requests without it.
    val jReq        = new DefaultFullHttpRequest(jVersion, method, uri, content)
    jReq.headers().set(headers)

    jReq
  }
}
