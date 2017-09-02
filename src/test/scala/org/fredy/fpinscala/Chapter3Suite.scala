package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter3Suite extends FunSuite {
  test("List.tail") {
    val list = Chapter3.List.tail(Chapter3.List(1, 2, 3, 4, 5))
    assert(Chapter3.List(2, 3, 4, 5) == list)
  }

  test("List.setHead") {
    val list = Chapter3.List.setHead(Chapter3.List(1, 2, 3, 4, 5), 10)
    assert(Chapter3.List(10, 2, 3, 4, 5) == list)
  }

  test("List.drop") {
    val list = Chapter3.List.drop(Chapter3.List(1, 2, 3, 4, 5), 2)
    assert(Chapter3.List(3, 4, 5) == list)
  }

  test("List.dropWhile") {
    val list1 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x < 3)
    assert(Chapter3.List(3, 4, 5) == list1)

    val list2 = Chapter3.List.dropWhile(Chapter3.List(1, 2, 3, 4, 5), (x: Int) => x <= 3)
    assert(Chapter3.List(4, 5) == list2)
  }

  test("List.init") {
    val list1 = Chapter3.List.init(Chapter3.List(1, 2, 3, 4, 5))
    assert(Chapter3.List(1, 2, 3, 4) == list1)

    val list2 = Chapter3.List.init(Chapter3.List(1, 2))
    assert(Chapter3.List(1) == list2)
  }

  test("List.length") {
    val list1 = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.length(list1) == 5)

    val list2 = Chapter3.List()
    assert(Chapter3.List.length(list2) == 0)
  }

  test("List.foldLeft") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.foldLeft(list, 0)((b, a) => b + a) == 15)
  }

  test("List.foldRight") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.foldRight(list, 0)((a, b) => a + b) == 15)
  }

  test("List.sum") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.sum(list) == 15)
  }

  test("List.product") {
    val list = Chapter3.List(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(Chapter3.List.product(list) == 120)
  }

  test("List.reverse") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.reverse(list) == Chapter3.List(5, 4, 3, 2, 1))
  }

  test("List.append") {
    val list1 = Chapter3.List(1, 2)
    val list2 = Chapter3.List(3, 4, 5)
    assert(Chapter3.List.append(list1, list2) == Chapter3.List(1, 2, 3, 4, 5))
  }

  test("List.concat") {
    val list = Chapter3.List(Chapter3.List(1, 2), Chapter3.List(3), Chapter3.List(4, 5))
    assert(Chapter3.List.concat(list) == Chapter3.List(1, 2, 3, 4, 5))
  }

  test("List.add") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.add(list) == Chapter3.List(2, 3, 4, 5, 6))
  }

  test("List.doubleToString") {
    val list = Chapter3.List(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(Chapter3.List.doubleToString(list) == Chapter3.List("1.0", "2.0", "3.0", "4.0", "5.0"))
  }

  test("List.map") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.map(list)(a => a * 2) == Chapter3.List(2, 4, 6, 8, 10))
  }

  test("List.filter") {
    val list = Chapter3.List(1, 2, 3, 4, 5)
    assert(Chapter3.List.filter(list)(a => a % 2 == 0) == Chapter3.List(2, 4))
  }

  test("List.flatMap") {
    val list = Chapter3.List(1, 2, 3)
    assert(Chapter3.List.flatMap(list)(i => Chapter3.List(i, i)) == Chapter3.List(1, 1, 2, 2, 3, 3))
  }

  test("List.zipWith") {
    val list1 = Chapter3.List(1, 2, 3)
    val list2 = Chapter3.List(4, 5, 6)
    assert(Chapter3.List.zipWith(list1, list2)((a, b) => a + b) == Chapter3.List(5, 7, 9))
  }

  test("List.hasSubsequence") {
    val list = Chapter3.List(1, 2, 3, 4)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(1, 2)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(2, 3)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(4)) == true)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(2, 1)) == false)
    assert(Chapter3.List.hasSubsequence(list, Chapter3.List(5)) == false)
  }

  test("Tree.size") {
    val tree = Chapter3.Branch(Chapter3.Leaf(1), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    assert(Chapter3.Tree.size(tree) == 5)
  }

  test("Tree.maximum") {
    val tree = Chapter3.Branch(Chapter3.Leaf(5), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    assert(Chapter3.Tree.maximum(tree) == 5)
  }

  test("Tree.depth") {
    val tree = Chapter3.Branch(Chapter3.Leaf(5), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    assert(Chapter3.Tree.depth(tree) == 3)
  }

  test("Tree.map") {
    val tree = Chapter3.Branch(Chapter3.Leaf(5), Chapter3.Branch(Chapter3.Leaf(2), Chapter3.Leaf(3)))
    val expected = Chapter3.Branch(Chapter3.Leaf(6), Chapter3.Branch(Chapter3.Leaf(3), Chapter3.Leaf(4)))
    assert(Chapter3.Tree.map(tree)(a => a + 1) == expected)
  }

  test("Tree.fold") {
    
  }
}