package zhttp.service.server

import io.netty.buffer.{Unpooled => JUnpooled}
import io.netty.handler.codec.http.websocketx.{WebSocketServerProtocolHandler => JWebSocketServerProtocolHandler}
import io.netty.handler.codec.http.{LastHttpContent => JLastHttpContent}
import zhttp.core.{JFullHttpRequest, _}
import zhttp.http._
import zhttp.service.Server.Settings
import zhttp.service._
import zio.Exit

/**
 * Helper class with channel methods
 */
@JSharable
final case class ServerRequestHandler[R](
  zExec: UnsafeChannelExecutor[R],
  settings: Settings[R, Throwable],
) extends JSimpleChannelInboundHandler[JFullHttpRequest](AUTO_RELEASE_REQUEST)
    with HttpMessageCodec {

  self =>

  /**
   * Tries to release the request byte buffer, ignores if it can not.
   */
  private def releaseOrIgnore(jReq: JFullHttpRequest): Boolean = jReq.release(jReq.content().refCnt())

  /**
   * Asynchronously executes the Http app and passes the response to the callback.
   */
  private def executeAsync(ctx: JChannelHandlerContext, jReq: JFullHttpRequest)(
    cb: Response[R, Throwable] => Unit,
  ): Unit =
    decodeJRequest(jReq) match {
      case Left(err)  => cb(err.toResponse)
      case Right(req) =>
        settings.http.execute(req).evaluate match {
          case HttpResult.Empty      => cb(Response.fromHttpError(HttpError.NotFound(Path(jReq.uri()))))
          case HttpResult.Success(a) => cb(a)
          case HttpResult.Failure(e) => cb(SilentResponse[Throwable].silent(e))
          case HttpResult.Effect(z)  =>
            zExec.unsafeExecute(ctx, z) {
              case Exit.Success(res)   => cb(res)
              case Exit.Failure(cause) =>
                cause.failureOption match {
                  case Some(Some(e)) => cb(SilentResponse[Throwable].silent(e))
                  case Some(None)    => cb(Response.fromHttpError(HttpError.NotFound(Path(jReq.uri()))))
                  case None          => {
                    ctx.close()
                    ()
                  }
                }
            }
        }
    }

  /**
   * Unsafe channel reader for HttpRequest
   */
  override def channelRead0(ctx: JChannelHandlerContext, jReq: JFullHttpRequest): Unit = {
    executeAsync(ctx, jReq) {
      case res @ Response.HttpResponse(_, _, content) =>
        content match {
          case HttpData.StreamData(data) =>
            ctx.write(encodeResponse(jReq.protocolVersion(), res), ctx.channel().voidPromise())
            zExec.unsafeExecute_(ctx) {
              for {
                _ <- data.foreachChunk(c => ChannelFuture.unit(ctx.writeAndFlush(JUnpooled.copiedBuffer(c.toArray))))
                _ <- ChannelFuture.unit(ctx.writeAndFlush(JLastHttpContent.EMPTY_LAST_CONTENT))
              } yield ()
            }
          case HttpData.CompleteData(_)  =>
            ctx.write(encodeResponse(jReq.protocolVersion(), res), ctx.channel().voidPromise())
          case HttpData.Empty            => ctx.write(encodeResponse(jReq.protocolVersion(), res), ctx.channel().voidPromise())
        }
        ()

      case res @ Response.SocketResponse(_) =>
        ctx
          .channel()
          .pipeline()
          .addLast(new JWebSocketServerProtocolHandler(res.socket.config.protocol.javaConfig))
          .addLast(WEB_SOCKET_HANDLER, ServerSocketHandler(zExec, res.socket.config))
        ctx.channel().eventLoop().submit(() => ctx.fireChannelRead(jReq))
        ()
    }
    releaseOrIgnore(jReq)
    ()
  }

  /**
   * Handles exceptions that throws
   */
  override def exceptionCaught(ctx: JChannelHandlerContext, cause: Throwable): Unit = {
    settings.error match {
      case Some(v) => zExec.unsafeExecute_(ctx)(v(cause).uninterruptible)
      case None    => ()
    }
    ctx.close()
    ()
  }
  override def channelReadComplete(ctx: JChannelHandlerContext): Unit = {
    ctx.flush()
    ()
  }

  override def channelUnregistered(ctx: JChannelHandlerContext): Unit = {
    ctx.close()
    ()
  }
}
