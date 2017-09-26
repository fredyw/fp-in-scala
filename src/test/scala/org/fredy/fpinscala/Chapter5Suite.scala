package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter5Suite extends FunSuite {
  test("Stream.toList") {
    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).toList == List(1, 2, 3))
  }

  test("Stream.take") {
    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).take(2).toList == List(1, 2))

    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).take(5).toList == List(1, 2, 3))
  }

  test("Stream.drop") {
    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).drop(1).toList == List(2, 3))

    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).drop(5).toList == List())
  }

  test("Stream.takeWhile") {
    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).takeWhile(a => a < 3).toList == List(1, 2))

    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).takeWhile(a => a < 5).toList == List(1, 2, 3))

    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).takeWhile(a => a < 0).toList == List())
  }

  test("Stream.forAll") {
    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).forAll(a => a < 5) == true)

    assert(Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).forAll(a => a < 3) == false)
  }
}
