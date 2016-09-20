package validator

import org.scalatest._

class ExampleSpec extends FunSuite {
  test("example1 - case class") {
    case class Foo(a: String, b: Int)

    val v1: Validation[Foo] = Validation(
      string("a") is minLength(1),
      int("b") is between(1, 3)
    ).as[Foo]

    val result = validate(Map("a" -> "A", "b" -> "1"), v1)
    assert(result == ValidationSuccess(Foo("A", 1)))

    val result2 = validate(Map("a" -> "A", "b" -> "0"), v1)
    assert(result2 == ValidationFailure.of("b" -> Seq(ValidationError("between", Seq("1", "3")))))
  }

  test("example2 - tuple") {
    val v1: Validation[(String, Int)] = Validation(
      string("a") is minLength(1),
      int("b") is between(1, 3)
    ).asTuple

    val result = validate(Map("a" -> "A", "b" -> "1"), v1)

    assert(result == ValidationSuccess(("A", 1)))
  }

  test("example3 - composing") {
    val v1 = Validation(
      string("a") is minLength(1),
      int("b") is between(1, 3)
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
      b <- int("b") is lessThanEq(2)
    } yield a + b

    val result = validate(Map("a" -> "1.1", "b" -> "2"), v1)

    assert(result == ValidationSuccess(3.1))
  }

  test("example5 - condition") {
    val v1: Validation[String] = boolean("flag").flatMap { flag =>
      if (flag) string("a") is equiv("A")
      else string("b") is equiv("B")
    }

    val result1 = validate(Map("flag" -> "true", "a" -> "A", "b" -> "B"), v1)
    assert(result1 == ValidationSuccess("A"))

    val result2 = validate(Map("flag" -> "false", "a" -> "Z", "b" -> "B"), v1)
    assert(result2 == ValidationSuccess("B"))
  }

  test("example6 - enum") {
    // Enum base classes
    trait Enum {
      type Value
      def value: Value
    }
    trait EnumCompanion[A <: Enum] {
      def valueOf(i: Byte): Option[A]
    }

    // My Enum classes
    sealed class Status(val value: Byte) extends Enum { type Value = Byte }
    case object Ok extends Status(0)
    case object Ng extends Status(1)

    object Status extends EnumCompanion[Status] {
      def valueOf(i: Byte): Option[Status] =
        if (i == 0) Some(Ok)
        else if (i == 1) Some(Ng)
        else None
    }

    // Defined Validation[Enum]
    def enum[A <: Enum](name: String, ec: EnumCompanion[A]): Validation[A] = {
      byte(name).map(ec.valueOf).transform {
        case Some(status) => ValidationSuccess(status)
        case None => ValidationFailure.of(name -> Seq(ValidationError("enum")))
      }
    }

    // run
    val v1 = enum("status", Status)
    assert(v1.run(Map("status" -> "0")) == ValidationSuccess(Ok))
    assert(v1.run(Map("status" -> "1")) == ValidationSuccess(Ng))
    assert(v1.run(Map("status" -> "2")) == ValidationFailure.of("status" -> Seq(ValidationError("enum"))))
  }

  test("example7 - or") {
    import shapeless.{:+:, Inl, Inr, CNil}

    val v1 = string("a") is minLength(1)
    val v2 = int("b") is equiv(2)
    val v3 = double("c") is equiv(3)
    val v: Validation[String :+: Int :+: Double :+: CNil] = v1 :+: v2 :+: v3
    val result = v.run(Map("a" -> "", "b" -> "2", "c" -> "3"))
    assert(result == ValidationSuccess(Inr(Inl(2))))
    assert(result.value.flatMap(_.select[Int]) == Some(2))
  }
}
