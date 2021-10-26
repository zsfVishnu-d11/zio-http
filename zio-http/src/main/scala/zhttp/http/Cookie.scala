package zhttp.http

import zhttp.http.Cookie.{HttpOnly, Secure}

import java.time.Instant
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.{Failure, Success, Try}
final case class Cookie(
  name: String,
  content: String,
  expires: Option[Instant] = None,
  domain: Option[String] = None,
  path: Option[Path] = None,
  secure: Secure = false,
  httpOnly: HttpOnly = false,
  maxAge: Option[Duration] = None,
  sameSite: Option[Cookie.SameSite] = None,
) { self =>

  /**
   * set expiry in Cookie
   */
  def @@(expires: Instant): Cookie = self.copy(expires = Some(expires))

  /**
   * set domain in Cookie
   */
  def @@(domain: String): Cookie = self.copy(domain = Some(domain))

  /**
   * set path in Cookie
   */
  def @@(path: Path): Cookie = self.copy(path = Some(path))

  /**
   * set cookie with secure
   */
  def @@(secure: Secure): Cookie = self.copy(secure = secure)

  /**
   * set max-age in Cookie
   */
  def @@(maxAge: Duration): Cookie = self.copy(maxAge = Some(maxAge))

  /**
   * set same-site in Cookie
   */
  def @@(sameSite: Cookie.SameSite): Cookie = self.copy(sameSite = Some(sameSite))

//  def @@(httpOnly: HttpOnly): Cookie = self.copy(httpOnly = httpOnly)

  /**
   * clears the cookie with empty content
   */
  def clearCookie: Cookie =
    copy(content = "", expires = Some(Instant.ofEpochSecond(0)))

  /**
   * set content in cookie
   */
  def setContent(v: String): Cookie = copy(content = v)

  /**
   * set httpOnly in Cookie
   */
  def withHttpOnly: Cookie = copy(httpOnly = true)

  /**
   * reset secure in Cookie
   */
  def resetSecure: Cookie = copy(secure = false)

  /**
   * reset httpOnly in Cookie
   */
  def resetHttpOnly: Cookie = copy(httpOnly = false)

  /**
   * remove expires in Cookie
   */
  def removeExpiry: Cookie = copy(expires = None)

  /**
   * remove domain in Cookie
   */
  def removeDomain: Cookie = copy(domain = None)

  /**
   * remove path in Cookie
   */
  def removePath: Cookie = copy(path = None)

  /**
   * remove max-age in Cookie
   */
  def removeMaxAge: Cookie = copy(maxAge = None)

  /**
   * remove same-site in Cookie
   */
  def removeSameSite: Cookie = copy(sameSite = None)

  /**
   * Cookie header to String
   */
  def asString: String = {
    val cookie = List(
      Some(s"$name=$content"),
      expires.map(e => s"Expires=$e"),
      maxAge.map(a => s"Max-Age=${a.toSeconds}"),
      domain.map(d => s"Domain=$d"),
      path.map(p => s"Path=${p.asString}"),
      if (secure) Some("Secure") else None,
      if (httpOnly) Some("HttpOnly") else None,
      sameSite.map(s => s"SameSite=${s.asString}"),
    )
    cookie.flatten.mkString("; ")
  }

}

object Cookie {
  type Secure   = Boolean
  type HttpOnly = Boolean

  sealed trait SameSite {
    def asString: String
  }
  object SameSite       {
    case object Lax    extends SameSite { def asString = "Lax"    }
    case object Strict extends SameSite { def asString = "Strict" }
    case object None   extends SameSite { def asString = "None"   }
  }

  /**
   * Parse cookie
   */
  private[zhttp] def parse(headerValue: String): Either[Throwable, Cookie] = {
    def splitNameContent(kv: String): (String, Option[String]) =
      kv.split("=", 2).map(_.trim) match {
        case Array(v1)     => (v1, None)
        case Array(v1, v2) => (v1, Some(v2))
        case _             => ("", None)
      }

    val cookieWithoutMeta = headerValue.split(";").map(_.trim)
    val (first, other)    = (cookieWithoutMeta.head, cookieWithoutMeta.tail)
    val (name, content)   = splitNameContent(first)
    var cookie            =
      if (name.trim == "" && content.isEmpty) Left(new IllegalArgumentException("Cookie can't be parsed"))
      else Right(Cookie(name, content.getOrElse("")))

    other.map(splitNameContent).map(t => (t._1.toLowerCase, t._2)).foreach {
      case ("expires", Some(v))  =>
        parseDate(v) match {
          case Left(_)      => cookie = Left(new IllegalArgumentException("expiry date cannot be parsed"))
          case Right(value) => cookie = cookie.map(_ @@ expiry(value))
        }
      case ("max-age", Some(v))  =>
        Try(v.toLong) match {
          case Success(maxAge) => cookie = cookie.map(_ @@ Duration(maxAge, SECONDS))
          case Failure(_)      => cookie = Left(new IllegalArgumentException("max-age cannot be parsed"))
        }
      case ("domain", v)         => cookie = cookie.map(_ @@ v.getOrElse(""))
      case ("path", v)           => cookie = cookie.map(_ @@ Path(v.getOrElse("")))
      case ("secure", _)         => cookie = cookie.map(_ @@ secure)
      case ("httponly", _)       => cookie = cookie.map(_.withHttpOnly)
      case ("samesite", Some(v)) =>
        v.trim.toLowerCase match {
          case "lax"    => cookie = cookie.map(_ @@ SameSite.Lax)
          case "strict" => cookie = cookie.map(_ @@ SameSite.Strict)
          case "none"   => cookie = cookie.map(_ @@ SameSite.None)
          case _        => None
        }
      case (_, _)                => cookie
    }
    cookie
  }

  def parseDate(v: String): Either[String, Instant] =
    Try(Instant.parse(v)) match {
      case Success(r) => Right(r)
      case Failure(e) => Left(s"Invalid http date: $v (${e.getMessage})")
    }

  def path(path: Path): Path                 = path
  def maxAge(maxAge: Duration): Duration     = maxAge
  def domain(domain: String): String         = domain
  def expiry(expires: Instant): Instant      = expires
  def sameSite(sameSite: SameSite): SameSite = sameSite
  def secure: Secure                         = true
//  def httpOnly: HttpOnly                     = true

}
