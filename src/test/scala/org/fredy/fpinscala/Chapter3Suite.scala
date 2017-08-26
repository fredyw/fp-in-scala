package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter3Suite extends FunSuite {
  test("tail") {
    val list = Chapter3.List.tail(Chapter3.List(1, 2, 3, 4, 5))
    assert(list == Chapter3.List(2, 3, 4, 5))
  }

  test("setHead") {
    val list = Chapter3.List.setHead(Chapter3.List(1, 2, 3, 4, 5), 10)
    assert(list == Chapter3.List(10, 2, 3, 4, 5))
  }

  test("drop") {
    val list = Chapter3.List.drop(Chapter3.List(1, 2, 3, 4, 5), 2)
    assert(list == Chapter3.List(3, 4, 5))
  }

  test("dropWhile") {
    val list1 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x < 3)
    assert(list1 == Chapter3.List(3, 4, 5))

    val list2 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x <= 3)
    assert(list2 == Chapter3.List(4, 5))
  }

  test("init") {
    val list1 = Chapter3.List.init(Chapter3.List(1, 2, 3, 4, 5))
    assert(list1 == Chapter3.List(1, 2, 3, 4))

    val list2 = Chapter3.List.init(Chapter3.List(1, 2))
    assert(list2 == Chapter3.List(1))
  }
}