package zhttp.service.server

import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, SimpleChannelInboundHandler}
import io.netty.handler.codec.http._
import zhttp.http._
import zhttp.service.Server.Config
import zhttp.service._
import zio.stream.ZStream
import zio.{Chunk, Exit, UIO, ZIO}

/**
 * Helper class with channel methods
 */
@Sharable
final case class ServerRequestHandler[R](
  zExec: UnsafeChannelExecutor[R],
  settings: Config[R, Throwable],
  serverTime: ServerTimeGenerator,
) extends SimpleChannelInboundHandler[FullHttpRequest](AUTO_RELEASE_REQUEST)
    with HttpMessageCodec
    with WebSocketUpgrade[R] {

  self =>

  /**
   * Asynchronously executes the Http app and passes the response to the callback.
   */
  private def executeAsync(ctx: ChannelHandlerContext, jReq: FullHttpRequest)(
    cb: Response[R, Throwable] => Unit,
  ): Unit =
    decodeJRequest(jReq, ctx) match {
      case Left(err)  => cb(err.toResponse)
      case Right(req) =>
        settings.app.execute(req).evaluate match {
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
  override def channelRead0(ctx: ChannelHandlerContext, jReq: FullHttpRequest): Unit = {
    executeAsync(ctx, jReq) { res =>
      if (self.canSwitchProtocol(res)) self.initializeSwitch(ctx, res)
      else {
        unsafeWriteAnyResponse(res)(ctx)
        res.data match {
          case HttpData.Empty =>
            unsafeWriteAndFlushLastEmptyContent()(ctx)

          case data @ HttpData.Text(_, _) =>
            unsafeWriteLastContent(data.encodeAndCache(res.attribute.memoize))(ctx)

          case HttpData.BinaryByteBuf(data) => unsafeWriteLastContent(data)(ctx)

          case data @ HttpData.BinaryChunk(_) =>
            unsafeWriteLastContent(data.encodeAndCache(res.attribute.memoize))(ctx)

          case HttpData.BinaryStream(stream) => {
            writeStreamContent(stream.mapChunks(a => Chunk(Unpooled.copiedBuffer(a.toArray))))(ctx)
            ()
          }
        }
      }
    }
  }

  /**
   * Writes last empty content to the Channel
   */
  private def unsafeWriteAndFlushLastEmptyContent()(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT, ctx.voidPromise()): Unit
  }

  /**
   * Writes any response to the Channel
   */
  private def unsafeWriteAnyResponse[A](res: Response[R, Throwable])(ctx: ChannelHandlerContext): Unit = {
    ctx.write(decodeResponse(res), ctx.voidPromise()): Unit
  }

  /**
   * Writes ByteBuf data to the Channel
   */
  private def unsafeWriteLastContent[A](data: ByteBuf)(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(new DefaultLastHttpContent(data), ctx.voidPromise()): Unit
  }
  private def decodeResponse(res: Response[_, _]): HttpResponse                          = {
    if (res.attribute.memoize) decodeResponseCached(res) else decodeResponseFresh(res)
  }

  private def decodeResponseCached(res: Response[_, _]): HttpResponse = {
    val cachedResponse = res.cache
    // Update cache if it doesn't exist OR has become stale
    // TODO: add unit tests for server-time
    if (cachedResponse == null || (res.attribute.serverTime && serverTime.canUpdate())) {
      val jRes = decodeResponseFresh(res)
      res.cache = jRes
      jRes
    } else cachedResponse
  }

  private def decodeResponseFresh(res: Response[_, _]): HttpResponse = {
    val jHeaders = Header.disassemble(res.getHeaders)
    if (res.attribute.serverTime) jHeaders.set(HttpHeaderNames.DATE, serverTime.refreshAndGet())
    new DefaultHttpResponse(HttpVersion.HTTP_1_1, res.status.asJava, jHeaders)
  }

  /**
   * Writes Binary Stream data to the Channel
   */
  private def writeStreamContent[A](stream: ZStream[R, Throwable, ByteBuf])(ctx: ChannelHandlerContext) = {
    stream.process.map { pull =>
      def loop: ZIO[R, Throwable, Unit] = pull
        .foldM(
          {
            case None        => UIO(ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT, ctx.voidPromise())).unit
            case Some(error) => ZIO.fail(error)
          },
          chunks =>
            for {
              _ <- ZIO.foreach_(chunks)(buf => UIO(ctx.write(new DefaultHttpContent(buf), ctx.voidPromise())))
              _ <- UIO(ctx.flush())
              _ <- loop
            } yield (),
        )

      loop
    }.useNow.flatten
  }

//      case res @ Response.HttpResponse(_, _, content) =>
//        ctx.write(encodeResponse(jReq.protocolVersion(), res), ctx.channel().voidPromise())
//        releaseOrIgnore(jReq)
//        content match {
//          case HttpData.BinaryStream(data)  =>
//            zExec.unsafeExecute_(ctx) {
//              for {
//                _ <- data.foreachChunk(c => ChannelFuture.unit(ctx.writeAndFlush(Unpooled.copiedBuffer(c.toArray))))
//                _ <- ChannelFuture.unit(ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT))
//              } yield ()
//            }
//          case HttpData.BinaryChunk(data)   =>
//            ctx.write(Unpooled.copiedBuffer(data.toArray), ctx.channel().voidPromise())
//            ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
//          case HttpData.Empty               => ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
//          case HttpData.Text(text, charset) =>
//            ctx.write(Unpooled.copiedBuffer(text.getBytes(charset)), ctx.channel().voidPromise())
//            ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
//          case HttpData.BinaryByteBuf(data) =>
//            ctx.write(Unpooled.copiedBuffer(data), ctx.channel().voidPromise())
//            ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
//        }
//        ()
//
//      case res @ Response.SocketResponse(_) =>
//        ctx
//          .channel()
//          .pipeline()
//          .addLast(new WebSocketServerProtocolHandler(res.socket.config.protocol.javaConfig))
//          .addLast(WEB_SOCKET_HANDLER, ServerSocketHandler(zExec, res.socket.config))
//        ctx.channel().eventLoop().submit(() => ctx.fireChannelRead(jReq))
//        ()
//    }
//  }

  /**
   * Handles exceptions that throws
   */
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    settings.error match {
      case Some(v) => zExec.unsafeExecute_(ctx)(v(cause).uninterruptible)
      case None    => {
        ctx.fireExceptionCaught(cause)
        ()
      }
    }
  }

}
