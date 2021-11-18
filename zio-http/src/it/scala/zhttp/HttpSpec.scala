package zhttp

import zhttp.http._
import zhttp.service._
import zio.test.Assertion._
import zio.test._

object HttpSpec {
  def testSuite(implicit addr: String, port: Int) = suite("HttpSpec") {
    testM("200 ok on /") {
      val response = Client.request(s"http://${addr}:${port}")

      assertM(response.map(_.status))(equalTo(Status.OK))
    } +
      testM("201 created on /post") {
        val url      = URL(Path.apply(), URL.Location.Absolute(Scheme.HTTP, addr, port))
        val endpoint = (Method.POST, url)

        val response = Client.request(Client.ClientParams(endpoint))

        assertM(response.map(_.status))(equalTo(Status.CREATED))
      }
  }
}
