package zhttp.service.server

import java.text.SimpleDateFormat
import java.util.Date

case class ServerTimeGenerator() {
  private val formatter          = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z")
  private var last: Long         = System.currentTimeMillis()
  private var lastString: String = formatter.format(new Date(last))

  def get: String = lastString

  def refreshAndGet: String = {
    refresh()
    get
  }

  def refresh(): Boolean = {
    val now = System.currentTimeMillis()
    if (now - last >= 1000) {
      last = now
      lastString = formatter.format(new Date(last))
      true
    } else {
      false
    }
  }
}

object ServerTimeGenerator {
  def make: ServerTimeGenerator = new ServerTimeGenerator()
}
