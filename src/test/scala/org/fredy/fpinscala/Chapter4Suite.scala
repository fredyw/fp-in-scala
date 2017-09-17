package org.fredy.fpinscala

import org.scalatest.FunSuite

class Chapter4Suite extends FunSuite {
  test("Option.map") {
    assert(Chapter4.Some("Hello World") == Chapter4.Some("Hello").map(a => a + " World"))
  }

  test("Option.getOrElse") {
    assert("Hello" == Chapter4.Some("Hello").getOrElse("Bye"))
    assert("Bye" == Chapter4.None.getOrElse("Bye"))
  }

  test("Option.flatMap") {
    assert(Chapter4.Some("Hello1") == Chapter4.Some(1).flatMap(a => Chapter4.Some("Hello" + a)))
    assert(Chapter4.None == Chapter4.None.flatMap(a => Chapter4.Some("Hello" + a)))
  }

  test("Option.orElse") {
    assert(Chapter4.Some("Hello") == Chapter4.Some("Hello").orElse(Chapter4.Some("Bye")))
    assert(Chapter4.Some("Bye") == Chapter4.None.orElse(Chapter4.Some("Bye")))
  }

  test("Option.filter") {
    assert(Chapter4.Some(1) == Chapter4.Some(1).filter(a => a == 1))
    assert(Chapter4.None == Chapter4.None.filter(a => a == 1))
  }

  test("Option.variance") {
    assert(Chapter4.Some(2.0) == Chapter4.Option.variance(List(1, 2, 3, 4, 5)))
    assert(Chapter4.None == Chapter4.Option.variance(List()))
  }

  test("Option.map2") {
    assert(Chapter4.Some(3) == Chapter4.Option.map2(Chapter4.Some(1), Chapter4.Some(2))((x, y) => x + y))
  }

  test("Option.sequence") {
    assert(Chapter4.Some(List(1, 2)) == Chapter4.Option.sequence(List(Chapter4.Some(1), Chapter4.Some(2))))
    assert(Chapter4.None == Chapter4.Option.sequence(List(Chapter4.None, Chapter4.Some(2))))
  }

  test("Option.traverse") {
    assert(Chapter4.Some(List(1, 2)) == Chapter4.Option.traverse(List(1, 2))(a => Chapter4.Some(a)))
    assert(Chapter4.None == Chapter4.Option.traverse(List(1, 2))(a => Chapter4.None))
  }

  test("Either.map") {
    assert(Chapter4.Right(2) == Chapter4.Right(1).map((a) => a + 1))
    assert(Chapter4.Left(1) == Chapter4.Left(1).map((a) => a))
  }

  test("Either.flatMap") {
    assert(Chapter4.Right(2) == Chapter4.Right(1).flatMap((a) => Chapter4.Right(a + 1)))
    assert(Chapter4.Left(1) == Chapter4.Left(1).flatMap((a) => Chapter4.Left(a)))
    assert(Chapter4.Left(1) == Chapter4.Right(1).flatMap((a) => Chapter4.Left(a)))
  }

  test("Either.orElse") {
    assert(Chapter4.Right(1) == Chapter4.Right(1).orElse(Chapter4.Left(100)))
    assert(Chapter4.Left(100) == Chapter4.Left(1).orElse(Chapter4.Left(100)))
  }

  test("Either.map2") {
    assert(Chapter4.Right(3) == Chapter4.Right(1).map2(Chapter4.Right(2))((a, b) => a + b))
  }

  test("Either.sequence") {
    // TODO
  }

  test("Either.traverse") {
    // TODO
  }
}
