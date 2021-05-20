package zhttp.service.server

import io.netty.buffer.{Unpooled => JUnpooled}
import io.netty.handler.codec.http.websocketx.{WebSocketServerProtocolHandler => JWebSocketServerProtocolHandler}
import io.netty.handler.codec.http.{LastHttpContent => JLastHttpContent}
import zhttp.core.{JFullHttpRequest, _}
import zhttp.http._
import zhttp.service.Server.Settings
import zhttp.service._
import zio.Exit

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

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
              case Exit.Success(res)   => {
                cb(res)
                ()
              }
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
            ctx.write(encodeResponse(jReq.protocolVersion(), res, date), ctx.channel().voidPromise())
            zExec.unsafeExecute_(ctx) {
              for {
                _ <- data.foreachChunk(c => ChannelFuture.unit(ctx.writeAndFlush(JUnpooled.copiedBuffer(c.toArray))))
                _ <- ChannelFuture.unit(ctx.writeAndFlush(JLastHttpContent.EMPTY_LAST_CONTENT))
              } yield ()
            }
          case HttpData.CompleteData(_)  =>
            ctx.writeAndFlush(encodeResponse(jReq.protocolVersion(), res, date), ctx.channel().voidPromise())
          case HttpData.Empty            =>
            ctx.writeAndFlush(encodeResponse(jReq.protocolVersion(), res, date), ctx.channel().voidPromise())
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

  override def channelUnregistered(ctx: JChannelHandlerContext): Unit = {
    ctx.close()
    ()
  }
  @volatile
  var date = s"${DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now)}"

  import java.util.concurrent.Executors

  private val scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)

  scheduler.scheduleWithFixedDelay(
    new Runnable() {
      override def run(): Unit = {
        date = s"${DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now)}"
      }
    },
    1000,
    1000,
    TimeUnit.MILLISECONDS,
  )
}
