package org.fredy.fpinscala

import org.scalatest._

class Chapter2Suite extends FunSuite {
  test("fib") {
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 1)
    assert(Chapter2.fib(3) == 2)
    assert(Chapter2.fib(4) == 3)
    assert(Chapter2.fib(5) == 5)
    assert(Chapter2.fib(6) == 8)
  }

  test("isSorted") {
    assert(Chapter2.isSorted(Array(1, 1, 2, 3, 4, 5, 5), (a: Int, b: Int) => a <= b) == true)
    assert(Chapter2.isSorted(Array(1), (a: Int, b: Int) => a <= b) == true)
    assert(Chapter2.isSorted(Array(1, 2, 4, 3, 5, 1), (a: Int, b: Int) => a <= b) == false)
  }

  test("curry") {
    assert(Chapter2.curry((a: Int, b: Int) => a + b)(1)(2) == 3)
  }

  test("uncurry") {
    assert(Chapter2.uncurry((a: Int) => (b: Int) => (a + b))(1, 2) == 3)
  }
}
