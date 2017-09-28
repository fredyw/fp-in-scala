package org.fredy.fpinscala

import scala.annotation.tailrec

object Chapter5 {
  trait Stream[+A] {
    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def exists(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    def toList: List[A] = {
      @tailrec
      def f(s: Stream[A], accu: List[A]): List[A] = {
        s match {
          case Empty => accu
          case Cons(h, t) => f(t(), h() :: accu)
        }
      }
      f(this, List()).reverse
    }

    def take(n: Int): Stream[A] = {
      this match {
        case Empty => Stream.empty
        case Cons(h, t) if (n > 1) => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if (n == 1) => Stream.cons(h(), Stream.empty)
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Empty => Stream.empty
        case Cons(_, t) if (n > 1) => t().drop(n - 1)
        case Cons(_, t) if (n == 1) => t()
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)
    }

    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((h, t) => p(h) && t)
    }

    def headOption: Option[A] = {
      foldRight(None: Option[A])((h, _) => Some(h))
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)
    }

    def append[B>:A](s: => Stream[B]): Stream[B] = ???

    def flatMap[B](f: A => Stream[B]): Stream[B] = ???
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }
}
