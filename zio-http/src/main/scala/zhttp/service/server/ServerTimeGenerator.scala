package zhttp.service.server

import io.netty.handler.codec.http.{HttpHeaderNames, HttpHeaders, HttpResponse}

import java.text.SimpleDateFormat
import java.util.Date

class ServerTimeGenerator() {
  private val formatter                     = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z")
  @volatile
  var date                                  = new Date()
  def update(headers: HttpHeaders): Boolean = {
    headers.set(HttpHeaderNames.DATE, formatter.format(date))
    true
  }

  def update(response: HttpResponse): HttpResponse = {
    update(response.headers())
    response
  }
}

object ServerTimeGenerator {
  def make: ServerTimeGenerator = {
    new ServerTimeGenerator() { self =>
      val t    = new java.util.Timer()
      val task = new java.util.TimerTask {
        def run() = self.date = new Date()
      }
      t.schedule(task, 0, 1000L)
    }
  }
}
