package validator

import org.scalatest._

class ExampleSpec extends FunSuite {
  test("example1 - case class") {
    case class Foo(a: String, b: Int)

    val v1: Validation[Foo] = Validation(
      string("a") is minLength(1),
      int("b") is min(1) and max(3)
    ).as[Foo]

    val result = validate(Map("a" -> "A", "b" -> "1"), v1)

    assert(result == ValidationSuccess(Foo("A", 1)))
  }

  test("example2 - tuple") {
    val v1: Validation[(String, Int)] = Validation(
      string("a") is minLength(1),
      int("b") is min(1) and max(3)
    ).asTuple

    val result = validate(Map("a" -> "A", "b" -> "1"), v1)

    assert(result == ValidationSuccess(("A", 1)))
  }

  test("example3 - composing") {
    val v1 = Validation(
      string("a") is minLength(1),
      int("b") is min(1) and max(3)
    )
    val v2 = Validation(
      string("c"),
      double("d")
    )

    val v3 = (v1 :: v2).asTuple

    val result = validate(Map("a" -> "A", "b" -> "1", "c" -> "C", "d" -> "4.1"), v3)

    assert(result == ValidationSuccess(("A", 1, "C", 4.1d)))
  }

  test("example4 - for") {
    val v1: Validation[Double] = for {
      a <- double("a") is equiv(1.1)
      b <- int("b") is min(2)
    } yield a + b

    val result = validate(Map("a" -> "1.1", "b" -> "2"), v1)

    assert(result == ValidationSuccess(3.1))
  }
}
