package zhttp.experiment.internal

import io.netty.handler.codec.http.{HttpMessage, HttpResponse}
import zio.test.Assertion
import zio.test.Assertion.anything
import zio.test.AssertionM.Render.param

trait HttpMessageAssertion {
  implicit final class HttpMessageSyntax(m: HttpMessage) {
    def asString: String = m.toString.dropWhile(_ != '\n')
  }

  def isResponse[A](assertion: Assertion[HttpResponse]): Assertion[A] =
    Assertion.assertionRec("isResponse")(param(assertion))(assertion)({
      case msg: HttpResponse => Option(msg)
      case _                 => None
    })

  def statusIs[A](code: Int): Assertion[HttpResponse] =
    Assertion.assertion("statusIs")(param(code))(_.status().code() == code)

  def hasHeader[A](name: String, value: String, ignoreCase: Boolean = true): Assertion[HttpResponse] =
    Assertion.assertion("hasHeader")(param(s"$name: $value"))(_.headers().contains(name, value, ignoreCase))

  def isAnyResponse: Assertion[Any] = isResponse(anything)

}