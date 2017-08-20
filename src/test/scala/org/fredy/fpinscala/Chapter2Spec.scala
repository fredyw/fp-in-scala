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
}
