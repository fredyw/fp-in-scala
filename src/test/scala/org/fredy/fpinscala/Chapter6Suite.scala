package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter6Suite extends FunSuite {
  test("State.map") {
    assert(Chapter6.State.unit(1).map(a => a + 1).run("state")._1 == 2)
  }

  test("State.map2") {
    assert(Chapter6.State.unit(1).map2(Chapter6.State((s: String) => (2, s)))((a, b) => a + b)
      .run("state")._1 == 3)
  }

  test("State.flatMap") {
    assert(Chapter6.State((s: String) => (1, s)).flatMap(a => Chapter6.State.unit(a + 1))
      .run("state")._1 == 2)
  }
}
