package ru.yudnikov.balancer

import com.typesafe.config.Config

import scala.concurrent.Future
import scala.concurrent.stm._
import scala.concurrent.ExecutionContext.Implicits.global

trait Stateful extends Loggable {

  private lazy val state: Ref[State] = Ref(restore)
  protected def config: Config

  def reserve(client: String, throughput: Int): Future[Option[Map[String, Int]]] = Future {
    atomic { implicit txn =>
      val currentState = state.get
      currentState.reserve(client, throughput).map { newState =>
        state() = newState
        newState.reserved(client)
      }
    }
  }

  def release(client: String): Future[Unit] = Future {
    atomic { implicit txn =>
      val currentState = state.get
      state() = currentState.release(client)
    }
  }

  def getState: Future[State] = Future {
    atomic { implicit txn =>
      state.get
    }
  }

  def restore: State = {
    State(config)
  }

  def takeSnapshot(): Unit = {
    ???
  }

}
