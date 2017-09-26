package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter5Suite extends FunSuite {
  test("Stream.toList") {
    assert(List(1, 2, 3) == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).toList)
  }

  test("Stream.take") {
    assert(List(1, 2) == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).take(2).toList)

    assert(List(1, 2, 3) == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).take(5).toList)
  }

  test("Stream.drop") {
    assert(List(2, 3) == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).drop(1).toList)

    assert(List() == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).drop(5).toList)
  }
}
