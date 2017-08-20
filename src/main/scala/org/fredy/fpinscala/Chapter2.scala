package org.fredy.fpinscala

import scala.annotation.tailrec

object Chapter2 {
  def fib(n: Int): Int = {
    @tailrec
    def f(n: Int, i: Int, j: Int): Int = {
      if (n == 0) i
      else f(n - 1, j, i + j)
    }
    f(n, 0, 1)
  }
}
