package org.fredy.fpinscala

import org.scalatest._

class Chapter2Spec extends FlatSpec with Matchers {
  "fib" should "return fibonnaci number" in {
    Chapter2.fib(1) shouldEqual 1
    Chapter2.fib(2) shouldEqual 1
    Chapter2.fib(3) shouldEqual 2
    Chapter2.fib(4) shouldEqual 3
    Chapter2.fib(5) shouldEqual 5
    Chapter2.fib(6) shouldEqual 8
  }

  "isSorted" should "return true for sorted array" in {
    Chapter2.isSorted(Array(1, 1, 2, 3, 4, 5, 5), (a: Int, b: Int) => a <= b) shouldEqual true
    Chapter2.isSorted(Array(1), (a: Int, b: Int) => a <= b) shouldEqual true
  }

  "isSorted" should "return false for non-sorted array" in {
    Chapter2.isSorted(Array(1, 2, 4, 3, 5, 1), (a: Int, b: Int) => a <= b) shouldEqual false
  }
}
