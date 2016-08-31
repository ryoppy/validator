package validator

import org.scalatest._
import shapeless.HNil

class ValidationSpec extends FunSuite {
  val params = Map("a" -> "1")

  test("run") {
    assert(string("a").run(params) == ValidationSuccess("1"))
  }
  test("apply") {
    assert(string("a").apply("1") == ValidationSuccess("1"))
  }

  test("addRule/is/and") {
    assert((string("a") is minLength(1)).apply("a") == ValidationSuccess("a"))
    assert((string("a") is minLength(2)).apply("a") == ValidationFailure("a" -> Seq(ValidationError("minLength", Seq("2")))))

    assert((string("a") is minLength(1) and maxLength(3)).apply("abc") == ValidationSuccess("abc"))
    assert((string("a") is minLength(2) and maxLength(3)).apply("a") == ValidationFailure("a" -> Seq(ValidationError("minLength", Seq("2")))))
    assert((string("a") is minLength(2) and maxLength(3)).apply("abcd") == ValidationFailure("a" -> Seq(ValidationError("maxLength", Seq("3")))))
  }

  test("map") {
    assert(string("a").map(_ + "!").apply("a") == ValidationSuccess("a!"))
    assert(int("a").map(_ + "!").apply("a") == ValidationFailure("a" -> Seq(ValidationError("int"))))
  }

  test("flatMap") {
    assert(string("a").flatMap(_ => int("b")).apply("1") == ValidationSuccess(1))
    assert(int("a").flatMap(_ => int("b")).apply("a") == ValidationFailure("a" -> Seq(ValidationError("int"))))
    assert(string("a").flatMap(_ => int("b")).apply("a") == ValidationFailure("b" -> Seq(ValidationError("int"))))
  }

  test("for") {
    {
      val v = for {a <- string("a"); b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("AB"))
    }
    {
      val v = for {a <- int("a"); b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationFailure("a" -> Seq(ValidationError("int"))))
    }
    {
      val v = for {a <- int("a"); b <- int("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationFailure("a" -> Seq(ValidationError("int"))))
    }
    {
      val v = for {a <- int("a"); b <- int("b")} yield a + b
      assert(v.run(Map("a" -> "1", "b" -> "2")) == ValidationSuccess(3))
    }
    {
      val v = for {a <- int("a") if a == 1; b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "2", "b" -> "B")) == ValidationFailure("a" -> Seq(ValidationError("a"))))
    }
  }

  test("::") {
    assert((string("a") :: string("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("A" :: "B" :: HNil))
    assert((int("a") :: int("b")).run(Map("a" -> "1", "b" -> "B")) == ValidationFailure("b" -> Seq(ValidationError("int"))))
    assert((int("a") :: int("b")).run(Map("a" -> "A", "b" -> "1")) == ValidationFailure("a" -> Seq(ValidationError("int"))))
    assert((int("a") :: int("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure("b" -> Seq(ValidationError("int")), "a" -> Seq(ValidationError("int"))))
  }

  test("as") {
    case class Foo(a: String, b: String)
    assert((string("a") :: string("b")).as[Foo].run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess(Foo("A", "B")))
    assertDoesNotCompile("""(string("a") :: int("b")).as[Foo].run(Map("a" -> "A", "b" -> "B"))""")
  }

  test("asTuple") {
    assert((string("a") :: string("b")).asTuple.run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("A" -> "B"))
  }
}
