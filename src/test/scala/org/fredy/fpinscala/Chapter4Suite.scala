package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter4Suite extends FunSuite {
  test("Option.map") {
    assert(Chapter4.Some("Hello").map(a => a + " World") == Chapter4.Some("Hello World"))
  }

  test("Option.getOrElse") {
    assert(Chapter4.Some("Hello").getOrElse("Bye") == "Hello" )
    assert(Chapter4.None.getOrElse("Bye") == "Bye")
  }

  test("Option.flatMap") {
    assert(Chapter4.Some(1).flatMap(a => Chapter4.Some("Hello" + a)) == Chapter4.Some("Hello1"))
    assert(Chapter4.None.flatMap(a => Chapter4.Some("Hello" + a)) == Chapter4.None)
  }

  test("Option.orElse") {
    assert(Chapter4.Some("Hello").orElse(Chapter4.Some("Bye")) == Chapter4.Some("Hello"))
    assert(Chapter4.None.orElse(Chapter4.Some("Bye")) == Chapter4.Some("Bye"))
  }

  test("Option.filter") {
    assert(Chapter4.Some(1).filter(a => a == 1) == Chapter4.Some(1))
    assert(Chapter4.None.filter(a => a == 1) == Chapter4.None)
  }

  test("Option.variance") {
    assert(Chapter4.Option.variance(List(1, 2, 3, 4, 5)) == Chapter4.Some(2.0))
    assert(Chapter4.Option.variance(List()) == Chapter4.None)
  }

  test("Option.map2") {
    assert(Chapter4.Option.map2(Chapter4.Some(1), Chapter4.Some(2))((x, y) => x + y) == Chapter4.Some(3))
  }

  test("Option.sequence") {
    assert(Chapter4.Option.sequence(List(Chapter4.Some(1), Chapter4.Some(2))) == Chapter4.Some(List(1, 2)) )
    assert(Chapter4.Option.sequence(List(Chapter4.None, Chapter4.Some(2))) == Chapter4.None)
  }

  test("Option.traverse") {
    assert(Chapter4.Option.traverse(List(1, 2))(a => Chapter4.Some(a)) == Chapter4.Some(List(1, 2)) )
    assert(Chapter4.Option.traverse(List(1, 2))(a => Chapter4.None) == Chapter4.None)
  }

  test("Either.map") {
    assert(Chapter4.Right(1).map((a) => a + 1) == Chapter4.Right(2))
    assert(Chapter4.Left(1).map((a) => a) == Chapter4.Left(1))
  }

  test("Either.flatMap") {
    assert(Chapter4.Right(1).flatMap((a) => Chapter4.Right(a + 1)) == Chapter4.Right(2))
    assert(Chapter4.Left(1).flatMap((a) => Chapter4.Left(a)) == Chapter4.Left(1))
    assert(Chapter4.Right(1).flatMap((a) => Chapter4.Left(a)) == Chapter4.Left(1))
  }

  test("Either.orElse") {
    assert(Chapter4.Right(1).orElse(Chapter4.Left(100)) == Chapter4.Right(1))
    assert(Chapter4.Left(1).orElse(Chapter4.Left(100)) == Chapter4.Left(100))
  }

  test("Either.map2") {
    assert(Chapter4.Right(1).map2(Chapter4.Right(2))((a, b) => a + b) == Chapter4.Right(3))
  }

  test("Either.sequence") {
    assert(Chapter4.Either.sequence(List(Chapter4.Right(1), Chapter4.Right(2))) == Chapter4.Right(List(1, 2)) )
    assert(Chapter4.Either.sequence(List(Chapter4.Right(1), Chapter4.Left(2))) == Chapter4.Left(2))
  }

  test("Either.traverse") {
    assert(Chapter4.Either.traverse(List(1, 2))(a => Chapter4.Right(a + 1)) == Chapter4.Right(List(2, 3)))
    assert(Chapter4.Either.traverse(List(1, 2))(a => Chapter4.Left(1)) == Chapter4.Left(1))
  }
}
