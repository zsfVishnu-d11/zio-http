package zhttp.internal

import zhttp.http.Response.HttpResponse
import zhttp.http._
import zhttp.service.{ChannelFactory, Client, EventLoopGroup, Server}
import zhttp.socket.{Socket, WebSocketFrame}
import zio.stream.ZStream
import zio.{Ref, UIO, ZIO}

import java.nio.file.Paths
import java.util.UUID

trait IntegrationTestExtensions {
  val addr = "localhost"
  val port = 8090

  implicit class URLSyntax(urlString: String) {
    def url: URL = URL(Path(urlString), URL.Location.Absolute(Scheme.HTTP, addr, port))
  }

  implicit class URLSyntaxPath(path: Path) {
    def url: URL = URL(path, URL.Location.Absolute(Scheme.HTTP, addr, port))
  }

  implicit class ResponseSyntax[R, E](zResponse: ZIO[R, E, HttpResponse[R, E]]) {
    def getStatus: ZIO[R, E, Status] = zResponse.map(_.status)

    def getHeaderNames: ZIO[R, E, List[CharSequence]] = zResponse
      .map(response => response.headers.map(_.name.toString))

    def getContent: ZIO[R, E, HttpData[R, E]] = zResponse.map(_.content)
  }

  def request(
               pathString: String = "/"
               , method: Method = Method.GET
             ): ZIO[EventLoopGroup with ChannelFactory, Throwable, UHttpResponse] = {
    val endpoint: Endpoint = (method, pathString.url)
    Client.request(endpoint)
  }

  def requestPath(
                   path: Path
                   , method: Method = Method.GET
                 ) = {
    val endpoint: Endpoint = (method, path.url)
    Client.request(endpoint)
  }

  def requestPathWUUID(
                        uuid: UUID
                   , path: Path
                   , method: Method = Method.GET
                 ) = {
    val p = !! / uuid.toString / (path.asString)
    val endpoint: Endpoint = (method, p.url)
    Client.request(endpoint)
  }

  type Route[R,E] = PartialFunction[Request, Response[R, E]]

  case class Id(uuid: UUID)

  import scala.collection._
  case class DynamicHttpApps[R, E](ref: Ref[mutable.Map[Id, HttpApp[R, E]]]) {
    self =>
    def set(app: HttpApp[R, E]): UIO[Id] = {
      val id = Id(UUID.randomUUID())
      self.ref
        .get
        .flatMap { m =>
          m.update(id,app)
          zio.ZIO.succeed(id)
        }
    }

    def get(id: Id): UIO[HttpApp[R, E]] =
      self.ref
        .get
        .map(
          _.get(id)
            .getOrElse(HttpApp.empty)
        )

    def delete(id: Id) = ???
  }

  val dServer: ZIO[Any, Nothing, DynamicHttpApps[Any, Throwable]] =
    Ref
      .make(scala.collection.mutable.Map.empty[Id, HttpApp[Any, Throwable]])
      .map(r => DynamicHttpApps(r))

  import zhttp.http._
  def app: Http[Any, Throwable, Request, Response[Any, Throwable]] =
    Http
      .collectM [Request]{ case req @ _ -> !! / id / _ =>
        dServer.map{ d =>
          d.get(Id(UUID.fromString(id)))
            .flatMap {
              httpApp =>
                httpApp(req)
                  .mapError(err => new Throwable(s"ERRROR: $err"))
            }
        }.flatten
      }

  val managedServer = Server.make(Server.port(8090) ++ Server.app(app)).orDie

  def extractResponse(
                       route: Route[Any,Nothing],
                       path: Path,
                       method: Method = Method.GET) = for {
    ds <- dServer
    httpApp = HttpApp.collect { route }
    id <- ds.set(httpApp)
    res <- requestPathWUUID(id.uuid, path, method )
  } yield (res)


  def file = HttpData.fromStream(ZStream.fromFile(Paths.get("README.md")))

  def socket = Socket.collect[WebSocketFrame] { case WebSocketFrame.Ping =>
    ZStream.succeed(WebSocketFrame.pong)
  }

}
