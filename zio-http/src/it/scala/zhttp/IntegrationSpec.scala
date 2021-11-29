package zhttp

import zhttp.http.Status._
import zhttp.http._
import zhttp.internal.{IntegrationTestAssertions, IntegrationTestExtensions}
import zhttp.service.{ChannelFactory, EventLoopGroup}
import zhttp.service.server.ServerChannelFactory
import zio.duration._
import zio.test.TestAspect._
import zio.test._

object IntegrationSpec
  extends DefaultRunnableSpec
    with IntegrationTestExtensions
    with IntegrationTestAssertions {

  def env    =
    EventLoopGroup.auto() ++ ChannelFactory.auto ++ ServerChannelFactory.auto ++ zio.clock.Clock.live ++ zio.blocking.Blocking.live

  def spec = suite("IntegrationSpec")(
    statusCodeSpec,
  ).provideCustomLayer(env) @@ timeout(30 seconds)

  def statusCodeSpec = suiteM("StatusCodeSpec") {
    managedServer
      .as(List(
        testM("200 ok on /[GET]") {
          // define a route
          val route: Route[Any,Nothing] = {
            case Method.GET ->  !! / _ / "dummy"  =>
              println(s"INVOKING GET ENDPOINT")
              Response.ok
          }
          // extract response
          val zResponse = extractResponse(route, !! / "dummy")
          // assert
          assertM(zResponse.getStatus)(status(OK))
        } +
          testM("201 created on /[POST]") {
            val route: Route[Any,Nothing] = {
              case Method.POST -> !! / _ / "dummy"  =>
                println(s"INVOKING POST ENDPOINT")
                Response.status(Status.CREATED)
            }
            val zResponse = extractResponse(
              route,
              !! / "dummy",
              Method.POST
            )
            assertM(zResponse.getStatus)(status(CREATED))
          } +
          testM("204 no content ok on /[PUT]") {
            val route: Route[Any,Nothing] = {
              case Method.PUT -> !! / _ / "dummy" =>
                println(s"INVOKING PUT ENDPOINT")
                Response.status(NO_CONTENT)
            }
//            val zResponse = request("/", Method.PUT)
            val zResponse = extractResponse(route, !! / "dummy", Method.PUT)
            assertM(zResponse.getStatus)(status(NO_CONTENT))
          } +
          testM("204 no content ok on /[DELETE]") {
            val route: Route[Any,Nothing] = {
              case Method.DELETE -> !! / _ / "dummy" =>
                println(s"INVOKING DELETE ENDPOINT")
                Response.status(NO_CONTENT)
            }
//            val zResponse = request("/", Method.DELETE)
            val zResponse = extractResponse(route, !! / "dummy", Method.DELETE)
            assertM(zResponse.getStatus)(status(NO_CONTENT))
          } +
          testM("404 on a random route") {
            checkM(Gen.int(1, 2).flatMap(Gen.stringN(_)(Gen.alphaNumericChar))) { randomUrl =>
              val zResponse = request(randomUrl)
              assertM(zResponse.getStatus)(status(NOT_FOUND))
            }
          } +
          testM("400 bad request on /subscriptions without the connection upgrade header") {
            val route: Route[Any,Nothing] = {
              case Method.GET -> !! / _ / "subscriptions" =>
                println(s"INVOKING WS ENDPOINT")
                Response.socket(socket)
            }
            val zResponse = extractResponse(route, !! / "subscriptions")
//            val zResponse = requestPath(!! / "subscriptions")
            assertM(zResponse.getStatus)(status(BAD_REQUEST))
          } +
          testM("500 internal server error on /boom") {
            val route: Route[Any,Nothing] = {
              case Method.GET -> !! / _ / "boom" =>
                println(s"INVOKING INTERNAL SERVER ENDPOINT")
                Response.status(Status.INTERNAL_SERVER_ERROR)
            }
            val zResponse = extractResponse(route, !! / "boom")
//            val zResponse = requestPath(!! / "boom")
            assertM(zResponse.getStatus)(status(INTERNAL_SERVER_ERROR))
          } +
          testM("200 ok on Stream file") {
            val zResponse = requestPath(!! / "stream" / "file")
            assertM(zResponse.getStatus)(status(OK))
          } @@ ignore
      ))
      .useNow
  }
}
