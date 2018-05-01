package ru.yudnikov.balancer

import java.net.InetAddress

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, Uri}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.typesafe.config.{Config, ConfigFactory}
import org.json4s.jackson.Serialization
import org.json4s.{DefaultFormats, Formats, FullTypeHints}

import scala.concurrent.{ExecutionContext, Future}

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Daemon extends App
  with Stateful
  with Loggable {

  implicit val config: Config = ConfigFactory.load()
  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContext = system.dispatcher
  implicit val formats: Formats = DefaultFormats
  implicit val descOrd: Ordering[BigInt] = (x, y) => y compareTo x

  val host = config.getString("app.host")
  val port = config.getInt("app.port")

  var serverSource = Http().bind(host, port)

  def requestHandler(address: InetAddress): HttpRequest => Future[HttpResponse] = {
    case HttpRequest(GET, uri@Uri.Path("/balance"), _, _, _) =>
      balanceGetHandler(address, uri)
    case HttpRequest(POST, uri@Uri.Path("/end"), _, _, _) =>
      logger.debug(s"received GET $uri request")
      endPostHandler(address)
    case HttpRequest(GET, Uri.Path("/state"), _, _, _) =>
      getState map { state =>
        logger.debug(s"state = $state")
        HttpResponse(200, entity = HttpEntity(state.toString))
      }
    case _ =>
      Future(HttpResponse(404, entity = "Not found!"))
  }

  private def balanceGetHandler(address: InetAddress, uri: Uri): Future[HttpResponse] = {
    logger.debug(s"received GET $uri request")
    uri.query() match {
      case Uri.Query.Cons("throughput", amountStr, _) if amountStr.nonEmpty && amountStr.forall(_.isDigit) =>
        reserve(address.toString, amountStr.toInt) map {
          case Some(reserved) =>
            val answer = Serialization.write(reserved)
            logger.debug(s"answer = $answer")
            HttpResponse(200, entity = answer)
          case _ =>
            HttpResponse(500)
        }
      case x =>
        logger.error(s"wrong request parameters $x")
        Future(HttpResponse(500))
    }
  }

  private def endPostHandler(address: InetAddress): Future[HttpResponse] = {
    release(address.toString) map { _ =>
      HttpResponse(200, entity = s"success")
    }
  }

  val bindingFuture: Future[Http.ServerBinding] = serverSource.to(Sink.foreach { connection =>
    connection.handleWithAsyncHandler(requestHandler(connection.remoteAddress.getAddress), 10)
  }).run()

  logger.info(s"service is online at http://$host:$port")
  StdIn.readLine(s"press Enter to terminate...")

  Try(takeSnapshot())

  system.terminate()

}
