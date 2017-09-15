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

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(a) => a
        case None => default
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case Some(a) => this
        case None => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(a => if (f(a)) Some(a) else None)
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}
