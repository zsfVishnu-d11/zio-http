package zhttp.service

import io.netty.handler.codec.DecoderException
import io.netty.handler.ssl.SslContextBuilder
import zhttp.http._
import zhttp.internal.{AppCollection, HttpRunnableSpec}
import zhttp.service.client.ClientSSLHandler.ClientSSLOptions.CustomSSL
import zhttp.service.server.ServerSSLHandler.{ServerSSLOptions, ctxFromCert}
import zhttp.service.server._
import zio.ZIO
import zio.duration.durationInt
import zio.test.Assertion.equalTo
import zio.test.TestAspect._
import zio.test._

object SSLSpec extends HttpRunnableSpec(8088) {

  val serverSSL  = ctxFromCert(
    getClass().getClassLoader().getResourceAsStream("server.crt"),
    getClass().getClassLoader().getResourceAsStream("server.key"),
  )
  val clientSSL1 =
    SslContextBuilder.forClient().trustManager(getClass().getClassLoader().getResourceAsStream("server.crt")).build()
  val clientSSL2 =
    SslContextBuilder.forClient().trustManager(getClass().getClassLoader().getResourceAsStream("ss2.crt.pem")).build()

  override def spec = {
    suiteM("Server") {
      app.as(List(httpsSpec)).useNow
    }.provideCustomLayerShared(env) @@ timeout(30 seconds) @@ sequential
  }

  def httpsSpec = suite("SSLSpec") {
    testM("succeed when client has the server certificate") {
      val actual = statusHttps(!! / "success", CustomSSL(clientSSL1))
      assertM(actual)(equalTo(Status.OK))
    } @@ nonFlaky +
      testM("404 response when route not found") {
        val actual = statusHttps(!! / "random", CustomSSL(clientSSL1))
        assertM(actual)(equalTo(Status.NOT_FOUND))
      } @@ nonFlaky +
      testM("fail with DecoderException when client doesn't have the server certificate") {
        val actual = statusHttps(!! / "success", CustomSSL(clientSSL2)).catchSome(_ match {
          case _: DecoderException => ZIO.succeed("DecoderException")
        })
        assertM(actual)(equalTo("DecoderException"))
      } @@ nonFlaky +
      testM("Https Redirect when client makes http request") {
        val actual = statusHttps(!! / "success", CustomSSL(clientSSL1), Scheme.HTTP)
        assertM(actual)(equalTo(Status.PERMANENT_REDIRECT))
      } @@ nonFlaky +
      testM("succeed when client has default SSL") {
        val actual = statusHttps(!! / "success")
        assertM(actual)(equalTo(Status.OK))
      } @@ nonFlaky
  }

  private val env = EventLoopGroup.nio() ++ ChannelFactory.nio ++ ServerChannelFactory.nio ++ AppCollection.live

  private val staticApp = Http.collectM[Request] { case Method.GET -> !! / "success" =>
    ZIO.succeed(Response.ok)
  }

  private val app = serveHttps({ staticApp ++ AppCollection.app }, ServerSSLOptions(serverSSL))
}
