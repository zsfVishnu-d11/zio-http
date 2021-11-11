package zhttp.http

import io.netty.buffer.{ByteBufUtil, Unpooled}
import zio.{Chunk, Queue, Task, UIO, ZIO}

sealed trait Decoder[-R, +E, -A, +B] { self =>
  def decode(data: HttpData[Any, Throwable])(implicit ev: Chunk[Byte] <:< A): ZIO[R, Throwable, B] =
    Decoder.decode(self.asInstanceOf[Decoder[R, Throwable, Chunk[Byte], B]], data)
}

object Decoder {

  case object Text extends Decoder[Any, Nothing, Any, String]

  case class Step[R, E, S, A, B](state: S, next: (A, S, Boolean) => ZIO[R, E, (Option[B], S)])
      extends Decoder[R, E, A, B]

  private[zhttp] case class BackPressure[B](queue: Option[Queue[B]] = None, isFirst: Boolean = true) {
    self =>
    def withQueue(queue: Queue[B]): BackPressure[B] = if (self.queue.isEmpty) self.copy(queue = Option(queue)) else self
    def withFirst(cond: Boolean): BackPressure[B]   = if (cond == isFirst) self else self.copy(isFirst = cond)
  }

  val text: Decoder[Any, Nothing, Any, String] = Text

  def collect[S, A]: PartiallyAppliedCollect[S, A] = new PartiallyAppliedCollect(())

  final class PartiallyAppliedCollect[S, A](val unit: Unit) extends AnyVal {
    def apply[R, E, B](s: S)(f: (A, S, Boolean) => ZIO[R, E, (Option[B], S)]): Decoder[R, E, A, B] = Step(s, f)
  }

  def collectAll[A]: Decoder[Any, Nothing, A, Chunk[A]] = Decoder.collect[Chunk[A], A](Chunk.empty) {
    case (a, chunk, true)  => UIO((Option(chunk :+ a), chunk))
    case (a, chunk, false) => UIO((None, chunk :+ a))
  }

  val backPressure: Decoder[Any, Nothing, Chunk[Byte], Queue[Chunk[Byte]]] =
    Decoder.collect(BackPressure[Chunk[Byte]]()) { case (msg, state, _) =>
      for {
        queue <- state.queue.fold(Queue.bounded[Chunk[Byte]](1))(UIO(_))
        _     <- queue.offer(msg)
      } yield (if (state.isFirst) Option(queue) else None, state.withQueue(queue).withFirst(false))
    }

  sealed trait Error extends Throwable with Product { self =>
    override def getMessage(): String =
      self match {
        case Error.ContentDecodedOnce => "Content has already been decoded once."
        case Error.DecodeEmptyContent => "Can not decode empty content"
      }
  }

  private def decode[R, B](
    decoder: Decoder[R, Throwable, Chunk[Byte], B],
    data: HttpData[Any, Throwable],
  ): ZIO[R, Throwable, B] =
    data match {
      case HttpData.Empty                => ZIO.fail(Decoder.Error.DecodeEmptyContent)
      case HttpData.Text(data, charset)  =>
        decoder match {
          case Text                              => UIO(data.asInstanceOf[B])
          case step: Decoder.Step[_, _, _, _, _] =>
            step
              .asInstanceOf[Decoder.Step[R, Throwable, Any, Chunk[Byte], B]]
              .next(Chunk.fromArray(data.getBytes(charset)), step.state, true)
              .map(a => a._1)
              .flatMap(contentFromOption)
        }
      case HttpData.BinaryStream(stream) =>
        decoder match {
          case Text                              =>
            stream
              .fold(Unpooled.compositeBuffer())((s, b) => s.writeBytes(Array(b)))
              .map(b => b.toString(HTTP_CHARSET).asInstanceOf[B])
          case step: Decoder.Step[_, _, _, _, _] =>
            stream
              .fold(Unpooled.compositeBuffer())((s, b) => s.writeBytes(Array(b)))
              .map(a => a.array().take(a.writerIndex()))
              .map(Chunk.fromArray(_))
              .flatMap(
                step
                  .asInstanceOf[Decoder.Step[R, Throwable, Any, Chunk[Byte], B]]
                  .next(_, step.state, true)
                  .map(a => a._1)
                  .flatMap(contentFromOption),
              )
        }
      case HttpData.Binary(data)         =>
        decoder match {
          case Text                              => UIO((new String(data.toArray, HTTP_CHARSET)).asInstanceOf[B])
          case step: Decoder.Step[_, _, _, _, _] =>
            step
              .asInstanceOf[Decoder.Step[R, Throwable, Any, Chunk[Byte], B]]
              .next(data, step.state, true)
              .map(a => a._1)
              .flatMap(contentFromOption)
        }
      case HttpData.BinaryN(data)        =>
        decoder match {
          case Text                              => UIO(data.toString(HTTP_CHARSET).asInstanceOf[B])
          case step: Decoder.Step[_, _, _, _, _] =>
            step
              .asInstanceOf[Decoder.Step[R, Throwable, Any, Chunk[Byte], B]]
              .next(Chunk.fromArray(ByteBufUtil.getBytes(data)), step.state, true)
              .map(a => a._1)
              .flatMap(contentFromOption)
        }
    }
  private def contentFromOption[B](a: Option[B]): Task[B] = {
    a match {
      case Some(value) => ZIO(value)
      case None        => ZIO.fail(Decoder.Error.DecodeEmptyContent)
    }
  }

  object Error {
    case object ContentDecodedOnce extends Error
    case object DecodeEmptyContent extends Error
  }
}
