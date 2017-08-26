package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter3Suite extends FunSuite {
  test("tail") {
    val list = Chapter3.List.tail(Chapter3.List(1, 2, 3, 4, 5))
    assert(Chapter3.List(2, 3, 4, 5) == list)
  }

  test("setHead") {
    val list = Chapter3.List.setHead(Chapter3.List(1, 2, 3, 4, 5), 10)
    assert(Chapter3.List(10, 2, 3, 4, 5) == list)
  }

  test("drop") {
    val list = Chapter3.List.drop(Chapter3.List(1, 2, 3, 4, 5), 2)
    assert(Chapter3.List(3, 4, 5) == list)
  }
}
