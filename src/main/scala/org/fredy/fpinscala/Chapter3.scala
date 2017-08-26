package org.fredy.fpinscala

import scala.annotation.tailrec

object Chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(x, xs) => Cons(h, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else {
        l match {
          case Nil => Nil
          case Cons(_, xs) => drop(xs, n - 1)
        }
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    def init[A](l: List[A]): List[A] = {
      @tailrec
      def f(l: List[A], acc: List[A]): List[A] = {
        l match {
          case Nil => sys.error("init of empty list")
          case Cons(_, Nil) => acc
          case Cons(x, xs) => f(xs, append(acc, List(x)))
        }
      }
      f(l, List())
    }
  }
}
