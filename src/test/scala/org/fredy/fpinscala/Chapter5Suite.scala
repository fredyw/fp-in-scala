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
    Chapter5.Stream(1, 2, 3).flatMap(a => Chapter5.Stream(a + 1)).toList == List(2, 3, 4)
  }
}
