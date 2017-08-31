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
      foldRight(a1, a2)((a, b) => Cons(a, b))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
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

    def length[A](l: List[A]): Int = {
      foldLeft(l, 0)((a, _) => a + 1)
    }

    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def sum(ns: List[Int]) = {
      foldLeft(ns, 0)((b, a) => b + a)
    }

    def product(ns: List[Double]) = {
      foldLeft(ns, 1.0)((b, a) => b * a)
    }

    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, List[A]())((b, a) => Cons(a, b))
    }

    def concat[A](l: List[List[A]]): List[A] = {
      foldLeft(l, List[A]())((b, a) => append(b, a))
    }

    def add(l: List[Int]): List[Int] = {
      foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))
    }

    def map[A, B](l: List[A])(f: A => B): List[B] = ???
  }
}
