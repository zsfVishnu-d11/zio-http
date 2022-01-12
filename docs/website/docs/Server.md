# Server

This section will walk you through details about Server.

A Server is the component for the building and aggregating different components to finally serve requests.

### Binding Server to a socket address
You can bind server to Inet address in multiple ways 
```scala
Server.port(8090)
// or 
Server.bind(8090)
// or 
Server.bind("localhost",8090)
```
### Building and composing multiple HttpApp and mounting them
The services (HttpApp) that are mounted on this server to serve 
can be composed like this.
```scala
  private val fooBar: HttpApp[Any, Nothing] = Http.collect[Request] {
    case Method.GET -> !! / "foo" => Response.text("bar")
    case Method.GET -> !! / "bar" => Response.text("foo")
  }
  private val app = Http.collectM[Request] {
    case Method.GET -> !! / "random" => random.nextString(10).map(Response.text)
    case Method.GET -> !! / "utc"    => clock.currentDateTime.map(s => Response.text(s.toString))
  }
  val server =
    Server.port(PORT) ++              // Setup port
      Server.app(fooBar +++ app)      // Setup the Http app 
```
The services (HttpApp) that are mounted on this server to serve
can be composed and mounted using Server.app(app) like this.

### Starting the server in "forever" mode
```scala
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Configure thread count using CLI
    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

    // Create a new server
    server.make
      .use(_ =>
        // Waiting for the server to start
        console.putStrLn(s"Server started on port $PORT")

          // Ensures the server doesn't die after printing
          *> ZIO.never,
      )
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))
      .exitCode
  }
```

### Complete Example

```scala
import zhttp.http._
import zhttp.service._
import zhttp.service.server.ServerChannelFactory
import zio._

import scala.util.Try

object HelloWorldAdvanced extends App {
  // Set a port
  private val PORT = 8090

  private val fooBar: HttpApp[Any, Nothing] = Http.collect[Request] {
    case Method.GET -> !! / "foo" => Response.text("bar")
    case Method.GET -> !! / "bar" => Response.text("foo")
  }

  private val app = Http.collectM[Request] {
    case Method.GET -> !! / "random" => random.nextString(10).map(Response.text)
    case Method.GET -> !! / "utc"    => clock.currentDateTime.map(s => Response.text(s.toString))
  }

  private val server =
    Server.port(PORT) ++              // Setup port
      Server.paranoidLeakDetection ++ // Paranoid leak detection (affects performance)
      Server.app(fooBar +++ app)      // Setup the Http app

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Configure thread count using CLI
    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

    // Create a new server
    server.make
      .use(_ =>
        // Waiting for the server to start
        console.putStrLn(s"Server started on port $PORT")

          // Ensures the server doesn't die after printing
          *> ZIO.never,
      )
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))
      .exitCode
  }
}
```