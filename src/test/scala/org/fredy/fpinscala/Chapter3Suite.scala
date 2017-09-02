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
    assert(Chapter3.List.foldLeft(list, 0)((b, a) => b + a) == 15)
  }

  test("foldRight") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.foldRight(list, 0)((a, b) => a + b) == 15)
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

  test("map") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.map(list)(a => a * 2) == Chapter3.List(2, 4, 6, 8, 10))
  }

  test("filter") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.filter(list)(a => a % 2 == 0) == Chapter3.List(2, 4))
  }

  test("flatMap") {
    val list = Chapter3.List(1, 2, 3)
    assert(Chapter3.List.flatMap(list)(i => Chapter3.List(i, i)) == Chapter3.List(1, 1, 2, 2, 3, 3))
  }

  test("zipWith") {
    val list1 = Chapter3.List(1, 2, 3)
    val list2 = Chapter3.List(4, 5, 6)
    assert(Chapter3.List.zipWith(list1, list2)((a, b) => a + b) == Chapter3.List(5, 7, 9))
  }

  test("hasSubsequence") {
    val list = Chapter3.List(1, 2, 3, 4)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(1, 2)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(2, 3)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(4)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(2, 1)) == false)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(5)) == false)
  }

  test("size") {
    val tree = Chapter3.Branch(Chapter3.Leaf(1), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    assert(Chapter3.Tree.size(tree) == 5)
  }

  test("maximum") {
    val tree = Chapter3.Branch(Chapter3.Leaf(5), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    assert(Chapter3.Tree.maximum(tree) == 5)
  }
}