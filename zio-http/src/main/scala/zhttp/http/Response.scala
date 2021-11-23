package zhttp.http

import zhttp.socket.SocketApp

// RESPONSE
sealed trait Response[-R, +E] extends Product with Serializable { self => }

object Response extends ResponseHelpers {
  // Constructors
  final case class HttpResponse[-R, +E](status: Status, headers: List[Header], content: HttpData[R, E])
      extends Response[R, E]
      with HeaderExtension[Response[R, E]] { self =>

    override def getHeaders: List[Header] = headers

    /**
     * Updates the headers using the provided function
     */
    override def updateHeaders(f: List[Header] => List[Header]): Response[R, E] =
      self.copy(headers = f(self.getHeaders))
  }

  final case class SocketResponse[-R, +E](socket: SocketApp[R, E] = SocketApp.empty) extends Response[R, E]
}
