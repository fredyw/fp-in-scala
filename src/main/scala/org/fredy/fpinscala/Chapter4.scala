package org.fredy.fpinscala

object Chapter4 {
  // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
  import scala.{Either => _, Option => _, Some => _}

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(a) => Some(f(a))
        case None => None
      }
    }

    def getOrElse[B >: A](default: => B): B = ???

    def flatMap[B](f: A => Option[B]): Option[B] = ???

    def orElse[B >: A](ob: => Option[B]): Option[B] = ???

    def filter(f: A => Boolean): Option[A] = ???
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
