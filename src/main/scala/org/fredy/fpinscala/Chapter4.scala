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

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case (x :: xs) => x.flatMap(xx => sequence(xs).map(y => xx :: y))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case (x :: xs) => f(x).flatMap(xx => traverse(xs)(f).map(y => xx :: y))
    }
  }
}
