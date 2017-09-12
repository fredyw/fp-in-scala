package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter4Suite extends FunSuite {
  test("Option.map") {
    assert(Chapter4.Some("Hello World") == Chapter4.Some("Hello").map(a => a + " World"))
  }
}
