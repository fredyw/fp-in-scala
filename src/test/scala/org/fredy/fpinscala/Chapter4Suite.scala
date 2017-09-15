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

  test("Option.orElse") {
    assert(Chapter4.Some("Hello") == Chapter4.Some("Hello").orElse(Chapter4.Some("Bye")))
    assert(Chapter4.Some("Bye") == Chapter4.None.orElse(Chapter4.Some("Bye")))
  }

  test("Option.filter") {
    assert(Chapter4.Some(1) == Chapter4.Some(1).filter(a => a == 1))
    assert(Chapter4.None == Chapter4.None.filter(a => a == 1))
  }

  test("variance") {
    assert(Chapter4.Some(2.0) == Chapter4.variance(List(1, 2, 3, 4, 5)))
    assert(Chapter4.None == Chapter4.variance(List()))
  }
}
