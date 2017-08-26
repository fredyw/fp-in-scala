package org.fredy.fpinscala

import org.scalatest._

class Chapter2Suite extends FunSuite {
  test("fib") {
    assert(1 == Chapter2.fib(1))
    assert(1 == Chapter2.fib(2))
    assert(2 == Chapter2.fib(3))
    assert(3 == Chapter2.fib(4))
    assert(5 == Chapter2.fib(5))
    assert(8 == Chapter2.fib(6))
  }

  test("isSorted") {
    assert(true == Chapter2.isSorted(Array(1, 1, 2, 3, 4, 5, 5), (a: Int, b: Int) => a <= b))
    assert(true == Chapter2.isSorted(Array(1), (a: Int, b: Int) => a <= b))
    assert(false == Chapter2.isSorted(Array(1, 2, 4, 3, 5, 1), (a: Int, b: Int) => a <= b))
  }

  test("curry") {
    assert(3 == Chapter2.curry((a: Int, b: Int) => a + b)(1)(2))
  }

  test("uncurry") {
    assert(3 == Chapter2.uncurry((a: Int) => (b: Int) => (a + b))(1, 2))
  }

  test("compose") {
    assert(4 == Chapter2.compose((b: Int) => b * 2, (a: Int) => a + 1)(1))
  }
}
