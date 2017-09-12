package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter4Suite extends FunSuite {
  test("Option.map") {
    assert(Chapter4.Some("Hello World") == Chapter4.Some("Hello").map(a => a + " World"))
  }

  test("Option.getOrElse") {
    assert("Hello" == Chapter4.Some("Hello").getOrElse("Bye"))
    assert("Bye" == Chapter4.None.getOrElse("Bye"))
  }

  test("Option.flatMap") {
    assert(Chapter4.Some("Hello1") == Chapter4.Some(1).flatMap(a => Chapter4.Some("Hello" + a)))
    assert(Chapter4.None == Chapter4.None.flatMap(a => Chapter4.Some("Hello" + a)))
  }
}
