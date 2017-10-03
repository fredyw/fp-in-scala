package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter5Suite extends FunSuite {
  test("Stream.toList") {
    assert(Chapter5.Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("Stream.take") {
    assert(Chapter5.Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Chapter5.Stream(1, 2, 3).take(5).toList == List(1, 2, 3))
  }

  test("Stream.drop") {
    assert(Chapter5.Stream(1, 2, 3).drop(1).toList == List(2, 3))
    assert(Chapter5.Stream(1, 2, 3).drop(5).toList == List())
  }

  test("Stream.takeWhile") {
    assert(Chapter5.Stream(1, 2, 3).takeWhile(a => a < 3).toList == List(1, 2))
    assert(Chapter5.Stream(1, 2, 3).takeWhile(a => a < 5).toList == List(1, 2, 3))
    assert(Chapter5.Stream(1, 2, 3).takeWhile(a => a < 0).toList == List())
  }

  test("Stream.forAll") {
    assert(Chapter5.Stream(1, 2, 3).forAll(a => a < 5) == true)
    assert(Chapter5.Stream(1, 2, 3).forAll(a => a < 3) == false)
  }

  test("Stream.headOption") {
    assert(Chapter5.Stream(1, 2, 3).headOption == Some(1))
    assert(Chapter5.Empty.headOption == None)
  }

  test("Stream.map") {
    assert(Chapter5.Stream(1, 2, 3).map(a => a + 1).toList == List(2, 3, 4))
  }

  test("Stream.filter") {
    assert(Chapter5.Stream(1, 2, 3).filter(a => a % 2 == 0).toList == List(2))
  }

  test("Stream.append") {
    assert(Chapter5.Stream(1, 2).append(Chapter5.Stream(3, 4)).toList == List(1, 2, 3, 4))
  }

  test("Stream.flatMap") {
    assert(Chapter5.Stream(1, 2, 3).flatMap(a => Chapter5.Stream(a + 1)).toList == List(2, 3, 4))
  }

  test("Stream.constant") {
    assert(Chapter5.Stream.constant(1).take(3).toList == List(1, 1, 1))
  }

  test("Stream.from") {
    assert(Chapter5.Stream.from(1).take(3).toList == List(1, 2, 3))
  }

  test("Stream.fibs") {
    assert(Chapter5.Stream.fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("Stream.unfold") {
    assert(Chapter5.Stream.unfold(1)(a => Some((a, a + 1))).take(3).toList == List(1, 2, 3))
    assert(Chapter5.Stream.unfold(1)(a => if (a > 3) None else Some((a, a + 1))).toList == List(1, 2, 3))
  }

  test("Stream.ones") {
    assert(Chapter5.Stream.ones.take(3).toList == List(1, 1, 1))
  }

  test("Stream.zipWith") {
    assert(Chapter5.Stream(1, 2, 3).zipWith(Chapter5.Stream(4, 5, 6))((a, b) => a + b).toList == List(5, 7, 9))
  }

  test("Stream.zipAll") {
    assert(Chapter5.Stream(1, 2, 3).zipAll(Chapter5.Stream(4, 5, 6)).toList ==
      List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))))
  }

  test("Stream.startsWith") {
    assert(Chapter5.Stream(1, 2, 3).startsWith(Chapter5.Stream(1, 2)) == true)
    assert(Chapter5.Stream(1, 2).startsWith(Chapter5.Stream(1, 2)) == true)
    assert(Chapter5.Stream(2, 2, 3).startsWith(Chapter5.Stream(1, 2)) == false)
    assert(Chapter5.Stream(1, 2).startsWith(Chapter5.Stream(1, 2, 3)) == false)
  }

  test("Stream.tails") {
    assert(Chapter5.Stream(1, 2, 3).tails.map(a => a.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  test("Stream.scanRight") {
    assert(Chapter5.Stream(1, 2, 3).scanRight(0)((a, b) => a + b).toList == List(6, 5, 3, 0))
  }
}
