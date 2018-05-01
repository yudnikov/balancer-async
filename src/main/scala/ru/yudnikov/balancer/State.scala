package ru.yudnikov.balancer

import com.typesafe.config.Config
import scala.collection.JavaConverters._

import scala.annotation.tailrec
import scala.collection.mutable

case class State(available: List[(String, Int)], reserved: Map[String, Map[String, Int]] = Map()) extends Loggable {

  lazy val availableSum: Int = available.map(_._2).sum

  @tailrec
  final def reserve(client: String, requested: Int)(implicit ord: Ordering[Int]): Option[State] = {
    available.sortBy(_._2) match {
      case (server, availThroughput) :: tail if requested < availThroughput =>
        Some(State(server -> (availThroughput - requested) :: tail, reserved(client, server, requested)))
      case (server, availThroughput) :: tail if requested == availThroughput =>
        Some(State(tail, reserved(client, server, requested)))
      case (server, availThroughput) :: tail =>
        State(tail, reserved(client, server, availThroughput)).reserve(client, requested - availThroughput)
      case Nil =>
        logger.debug(s"We need more throughput, My Lord!")
        None
    }
  }

  private def reserved(client: String, server: String, amount: Int): Map[String, Map[String, Int]] = {
    val byClient = reserved.get(client).map { reservedByClient =>
      if (reservedByClient.contains(server)) {
        reservedByClient + (server -> (reservedByClient(server) + amount))
      } else {
        reservedByClient + (server -> amount)
      }
    }.getOrElse(Map(server -> amount))
    reserved + (client -> byClient)
  }

  def release(client: String): State = {
    reserved.get(client).map { reservedByClient =>
      val buffer = mutable.Map(available: _*)
      reservedByClient.foreach { case (server, amount) =>
        buffer.get(server) match {
          case Some(availableAmount) => buffer += server -> (amount + availableAmount)
          case _ => buffer += server -> amount
        }
      }
      State(buffer.toList, reserved - client)
    }.getOrElse(this)
  }
}

object State extends Loggable {
  def apply(config: Config)(implicit ordering: Ordering[BigInt]): State = {
    lazy val initAvailable: List[(String, Int)] = {
      config.getObject("servers").unwrapped().asScala.toList.map { case (ip, t) =>
        ip -> t.asInstanceOf[Int]
      }
    }
    State(initAvailable.sortBy(_._2), Map())
  }
}
