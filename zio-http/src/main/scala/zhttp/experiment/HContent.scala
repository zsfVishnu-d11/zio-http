package zhttp.experiment

import zio._
import zio.stream._

// Replacement for ZQueue
// Because ZStreams & ZQueue are going to be really really slow
// Specially for use cases of FullHttpRequest/Response
// This is not a general purpose streaming solution
sealed trait HContent[-R, +E, +A] {
  def map[B](bc: A => B): HContent[R, E, B]
  def mapChunk[B](bc: Chunk[A] => Chunk[B]): HContent[R, E, B]
  def mapM[R1, E1, B1](bc: A => ZIO[R1, E1, B1]): HContent[R1, E1, B1]
  def flatMap[R1 <: R, E1 >: E, C](f: A => HContent[R1, E1, C]): HContent[R1, E1, C]
  def runCollect: ZIO[R, E, Chunk[A]]
  def toZStream: ZStream[R, E, A]
}

object HContent {
  def empty: HContent[Any, Nothing, Nothing]              = ???
  def from[A](chunk: Chunk[A]): HContent[Any, Nothing, A] = ???
}
