package ru.yudnikov.balancer

import org.scalatest.{FlatSpec, Matchers}

class StateSuite extends FlatSpec with Matchers {

  val state = State(List("a" -> 5, "b" -> 10, "c" -> 1))

  "State" should "reserve depending on ordering" in {
    val maybeStateDesc = state.reserve("x", 2)((x, y) => y compareTo x)
    maybeStateDesc shouldEqual Some(State(List("b" -> 8, "a" -> 5, "c" -> 1), Map("x" -> Map("b" -> 2))))
    val maybeStateAsc = state.reserve("x", 2)((x, y) => x compareTo y)
    maybeStateAsc shouldEqual Some(State(List("a" -> 4, "b" -> 10), Map("x" -> Map("c" -> 1, "a" -> 1))))
  }

  it should "reserve all available when requested and no more" in {
    state.reserve("x", state.availableSum) shouldEqual Some(State(Nil, Map("x" -> Map("c" -> 1, "a" -> 5, "b" -> 10))))
    state.reserve("x", state.availableSum + 1) shouldEqual None
  }

  it should "release reserved throughput" in {
    val reservedState = state.reserve("x", state.availableSum).get
    val releasedState = reservedState.release("x")
    releasedState.availableSum shouldEqual state.availableSum
    releasedState.reserved shouldEqual Map()
  }

}
