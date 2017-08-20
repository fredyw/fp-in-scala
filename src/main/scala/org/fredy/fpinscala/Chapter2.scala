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

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def f(prev: Int, curr: Int): Boolean = {
      if (curr == as.length) true
      else if (!ordered(as(prev), as(curr))) false
      else f(curr, curr + 1)
    }
    f(0, 1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???
}
