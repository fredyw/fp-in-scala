package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter5Suite extends FunSuite {
  test("Stream.toList") {
    assert(List(1, 2, 3) == Chapter5.Cons(
      () => 1, () => Chapter5.Cons(
        () => 2, () => Chapter5.Cons(
          () => 3, () => Chapter5.Empty))).toList)
  }
}
