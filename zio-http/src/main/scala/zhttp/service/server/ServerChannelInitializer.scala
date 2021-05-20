package zhttp.service.server

import io.netty.handler.codec.http.{
  HttpRequestDecoder => JHttpRequestDecoder,
  HttpResponseEncoder => JHttpResponseEncoder,
  HttpServerKeepAliveHandler => JHttpServerKeepAliveHandler,
}
import io.netty.handler.flush.{FlushConsolidationHandler => JFlushConsolidationHandler}
import zhttp.core._
import zhttp.service._

/**
 * Initializes the netty channel with default handlers
 */
@JSharable
final case class ServerChannelInitializer(httpH: JChannelHandler, maxSize: Int) extends JChannelInitializer[JChannel] {
  override def initChannel(channel: JChannel): Unit = {
    channel
      .pipeline()
      .addLast(FLUSH_CONSOLIDATOR, new JFlushConsolidationHandler(256, false))
      .addLast(SERVER_ENCODER, new JHttpResponseEncoder)
      .addLast(SERVER_DECODER, new JHttpRequestDecoder(4096, 8192, 8192, false))
      .addLast(HTTP_KEEPALIVE_HANDLER, new JHttpServerKeepAliveHandler)
      .addLast(OBJECT_AGGREGATOR, new JHttpObjectAggregator(maxSize))
      .addLast(HTTP_REQUEST_HANDLER, httpH)
    ()
  }
}
