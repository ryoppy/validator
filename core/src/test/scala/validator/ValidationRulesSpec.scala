package validator

import org.scalatest._

class ValidationRulesSpec extends FunSuite {
  test("minLength") {
    assert(minLength(3).run("abc") == Right("abc"))
    assert(minLength(3).run("ab") == Left(ValidationError("minLength", Seq("3"))))
  }

  test("maxLength") {
    assert(maxLength(3).run("abc") == Right("abc"))
    assert(maxLength(3).run("abcd") == Left(ValidationError("maxLength", Seq("3"))))
  }

  test("min") {
    assert(min(3).run(3) == Right(3))
    assert(min(3).run(2) == Left(ValidationError("min", Seq("3"))))
  }

  test("max") {
    assert(max(3).run(3) == Right(3))
    assert(max(3).run(4) == Left(ValidationError("max", Seq("3"))))
  }

  test("equiv") {
    assert(equiv(3).run(3) == Right(3))
    assert(equiv(3).run(4) == Left(ValidationError("equiv", Seq("3"))))
  }

  test("equal") {
    assert(equal("abc").run("abc") == Right("abc"))
    assert(equal("abc").run("a") == Left(ValidationError("equal", Seq("abc"))))
  }

  test("email") {
    assert(email.run("foo@example.com") == Right("foo@example.com"))
    assert(email.run("foo") == Left(ValidationError("email")))
  }

  test("same") {
    assert(same("A").run("A") == Right("A"))
    assert(same("B").run("A") == Left(ValidationError("same", Seq("B"))))
  }
}
