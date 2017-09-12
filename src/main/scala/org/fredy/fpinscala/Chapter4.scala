package org.fredy.fpinscala

object Chapter4 {
  // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
  import scala.{Option => _, Some => _, Either => _, _}

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = ???

    def getOrElse[B >: A](default: => B): B = ???

    def flatMap[B](f: A => Option[B]): Option[B] = ???

    def orElse[B >: A](ob: => Option[B]): Option[B] = ???

    def filter(f: A => Boolean): Option[A] = ???
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
