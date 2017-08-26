package org.fredy.fpinscala

object Chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }
  }
}
