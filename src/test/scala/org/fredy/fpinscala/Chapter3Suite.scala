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

  test("dropWhile") {
    val list1 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x < 3)
    assert(Chapter3.List(3, 4, 5) == list1)

    val list2 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x <= 3)
    assert(Chapter3.List(4, 5) == list2)
  }

  test("init") {
    val list1 = Chapter3.List.init(Chapter3.List(1, 2, 3, 4, 5))
    assert(Chapter3.List(1, 2, 3, 4) == list1)

    val list2 = Chapter3.List.init(Chapter3.List(1, 2))
    assert(Chapter3.List(1) == list2)
  }

  test("length") {
    val list1 = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.length(list1) == 5)

    val list2 = Chapter3.List()
    assert(Chapter3.List.length(list2) == 0)
  }

  test("foldLeft") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.foldLeft(list, 0)((b, _) => b + 1) == 5)
  }
}