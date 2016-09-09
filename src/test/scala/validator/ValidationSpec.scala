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

  test("is/and") {
    assert((string("a") is minLength(1)).apply("a") == ValidationSuccess("a"))
    assert((string("a") is minLength(2)).apply("a") == ValidationFailure.of("a" -> Seq(ValidationError("minLength", Seq("2")))))

    assert((string("a") is minLength(1) and maxLength(3)).apply("abc") == ValidationSuccess("abc"))
    assert((string("a") is minLength(2) and maxLength(3)).apply("a") == ValidationFailure.of("a" -> Seq(ValidationError("minLength", Seq("2")))))
    assert((string("a") is minLength(2) and maxLength(3)).apply("abcd") == ValidationFailure.of("a" -> Seq(ValidationError("maxLength", Seq("3")))))

    val v1 = string("a").and("newName", "newRuleName") { a => a == "NEW" }
    assert(v1.apply("NEW") == ValidationSuccess("NEW"))
    assert(v1.apply("FOO") == ValidationFailure.of("newName" -> Seq(ValidationError("newRuleName"))))
  }

  test("add new validation rules") {
    val v1 = string("a").is(maxLength(1)).and("a is A") { a => a == "A" }
    assert(v1("A") == ValidationSuccess("A"))
    assert(v1("B") == ValidationFailure.of("a" -> Seq(ValidationError("a is A"))))
    assert(v1("BC") == ValidationFailure.of("a" -> Seq(ValidationError("maxLength", Seq("1")))))

    case class Foo(a: String, b: Int)
    val v2 = Validation(
      string("a") is minLength(1),
      int("b") is min(1)
    ).as[Foo]
      .and("foo", "foo is A1") { foo => foo.a == "A" && foo.b == 1 }

    assert(v2.run(Map("a" -> "A", "b" -> "1")) == ValidationSuccess(Foo("A", 1)))
    assert(v2.run(Map("a" -> "B", "b" -> "1")) == ValidationFailure.of("foo" -> Seq(ValidationError("foo is A1"))))
    assert(v2.run(Map("a" -> "A", "b" -> "0")) == ValidationFailure.of("b" -> Seq(ValidationError("min", Seq("1")))))
  }

  test("map") {
    assert(string("a").map(_ + "!").apply("a") == ValidationSuccess("a!"))
    assert(int("a").map(_ + "!").apply("a") == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
  }

  test("flatMap") {
    assert(string("a").flatMap(_ => int("b")).apply("1") == ValidationSuccess(1))
    assert(int("a").flatMap(_ => int("b")).apply("a") == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
    assert(string("a").flatMap(_ => int("b")).apply("a") == ValidationFailure.of("b" -> Seq(ValidationError("int"))))
  }

  test("for") {
    {
      val v = for {a <- string("a"); b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("AB"))
    }
    {
      val v = for {a <- int("a"); b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
    }
    {
      val v = for {a <- int("a"); b <- int("b")} yield a + b
      assert(v.run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
    }
    {
      val v = for {a <- int("a"); b <- int("b")} yield a + b
      assert(v.run(Map("a" -> "1", "b" -> "2")) == ValidationSuccess(3))
    }
    {
      val v = for {a <- int("a") if a == 1; b <- string("b")} yield a + b
      assert(v.run(Map("a" -> "2", "b" -> "B")) == ValidationFailure.of("a" -> Seq(ValidationError("a"))))
    }
  }

  test("::") {
    assert((string("a") :: string("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("A" :: "B" :: HNil))
    assert((int("a") :: int("b")).run(Map("a" -> "1", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("int"))))
    assert((int("a") :: int("b")).run(Map("a" -> "A", "b" -> "1")) == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
    assert((int("a") :: int("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("int")), "a" -> Seq(ValidationError("int"))))
  }

  test("|") {
    assert((int("a") | int("b")).run(Map("a" -> "1", "b" -> "2")) == ValidationSuccess(1))
    assert((int("a") | int("b")).run(Map("a" -> "A", "b" -> "2")) == ValidationSuccess(2))
    assert((int("a") | int("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("int"))))
  }

  test("as") {
    case class Foo(a: String, b: String)
    assert((string("a") :: string("b")).as[Foo].run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess(Foo("A", "B")))
    assertDoesNotCompile("""(string("a") :: int("b")).as[Foo].run(Map("a" -> "A", "b" -> "B"))""")
  }

  test("asTuple") {
    assert((string("a") :: string("b")).asTuple.run(Map("a" -> "A", "b" -> "B")) == ValidationSuccess("A" -> "B"))
  }

  test("sameValue") {
    assert(string("a").sameValue(string("b")).run(Map("a" -> "A", "b" -> "A")) == ValidationSuccess("A"))
    assert(string("a").sameValue(string("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("same", Seq("A")))))
  }

  test("transform") {
    assert(string("a").transform(a => ValidationSuccess("ABC")).run(Map("a" -> "A", "b" -> "A")) == ValidationSuccess("ABC"))
    assert(string("a").transform(a => ValidationFailure.of[String]("a" -> Seq(ValidationError("fuga")))).run(Map("a" -> "A", "b" -> "A")) == ValidationFailure.of("a" -> Seq(ValidationError("fuga"))))
  }

  test("changeName") {
    assert(((string("a") is equal("A")) | string("b") is equal("B")).changeName("b", "NewName").run(Map("a" -> "1", "b" -> "2")) == ValidationFailure.of("NewName" -> Seq(ValidationError("equal", Seq("B")))))
  }

  test("changeRuleName") {
    assert((string("a") sameValue string("b")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("same", Seq("A")))))
    assert((string("a") sameValue string("b") changeRuleName("b", "same", "this is not same values!")).run(Map("a" -> "A", "b" -> "B")) == ValidationFailure.of("b" -> Seq(ValidationError("this is not same values!", Seq("A")))))
  }

  test("rescue") {
    assert(string("a").is(equal("B"))
      .rescue { case ValidationFailure(xs) if xs.exists { case (name, _) => name == "a" } => ValidationSuccess("B") }.run(Map("a" -> "A"))
        == ValidationSuccess("B"))
  }

  test("optional is/and") {
    val v1 = optional(string("a")) is minLength(1) and maxLength(3)
    assert(v1.run(Map("a" -> "A")) == ValidationSuccess(Some("A")))
    assert(v1.run(Map()) == ValidationSuccess(None))
    assert(v1.run(Map("a" -> "")) == ValidationFailure.of("a" -> Seq(ValidationError("minLength", Seq("1")))))
    assert(v1.run(Map("a" -> "AAAA")) == ValidationFailure.of("a" -> Seq(ValidationError("maxLength", Seq("3")))))
  }
}
