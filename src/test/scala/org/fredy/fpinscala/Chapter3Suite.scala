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

  test("sum") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.sum(list) == 15)
  }

  test("product") {
    val list = Chapter3.List(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(Chapter3.List.product(list) == 120)
  }

  test("reverse") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.reverse(list) == Chapter3.List(5, 4, 3, 2, 1))
  }

  test("append") {
    val list1 = Chapter3.List(1, 2)
    val list2 = Chapter3.List(3, 4, 5)
    assert(Chapter3.List.append(list1, list2) == Chapter3.List(1, 2, 3, 4, 5))
  }

  test("concat") {
    val list = Chapter3.List(Chapter3.List(1, 2), Chapter3.List(3), Chapter3.List(4, 5))
    assert(Chapter3.List.concat(list) == Chapter3.List(1, 2, 3, 4, 5))
  }

  test("add") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.add(list) == Chapter3.List(2, 3, 4, 5, 6))
  }

  test("doubleToString") {
    val list = Chapter3.List(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(Chapter3.List.doubleToString(list) == Chapter3.List("1.0", "2.0", "3.0", "4.0", "5.0"))
  }
}