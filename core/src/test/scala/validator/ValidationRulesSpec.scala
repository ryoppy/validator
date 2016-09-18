package validator

import org.scalatest._

class ValidationRulesSpec extends FunSuite {
  test("required") {
    assert(required.run("abc") == Right("abc"))
    assert(required.run("") == Left(ValidationError("required")))
  }

  test("alpha") {
    assert(alpha.run("abc") == Right("abc"))
    assert(alpha.run("abc.") == Left(ValidationError("alpha")))
  }

  test("alphaNum") {
    assert(alphaNum.run("abc123") == Right("abc123"))
    assert(alphaNum.run("abc123.") == Left(ValidationError("alphaNum")))
  }

  test("minLength") {
    assert(minLength(3).run("abc") == Right("abc"))
    assert(minLength(3).run("ab") == Left(ValidationError("minLength", Seq("3"))))
  }

  test("maxLength") {
    assert(maxLength(3).run("abc") == Right("abc"))
    assert(maxLength(3).run("abcd") == Left(ValidationError("maxLength", Seq("3"))))
  }

  test("exactLength") {
    assert(exactLength(3).run("abc") == Right("abc"))
    assert(exactLength(3).run("abcd") == Left(ValidationError("exactLength", Seq("3"))))
  }

  test("email") {
    assert(email.run("foo@example.com") == Right("foo@example.com"))
    assert(email.run("foo.bar") == Left(ValidationError("email")))
  }

  test("ip") {
    assert(ip.run("127.0.0.1") == Right("127.0.0.1"))
    assert(ip.run("::1") == Right("::1"))
    assert(ip.run("IPアドレス") == Left(ValidationError("ip")))
  }

  test("ip4") {
    assert(ip4.run("127.0.0.1") == Right("127.0.0.1"))
    assert(ip4.run("::1") == Left(ValidationError("ip4")))
  }

  test("ip6") {
    assert(ip6.run("::1") == Right("::1"))
    assert(ip6.run("127.0.0.1") == Left(ValidationError("ip6")))
  }

  test("url") {
    assert(url.run("http://example.com") == Right("http://example.com"))
    assert(url.run("foo") == Left(ValidationError("url")))
  }

  test("regex") {
    assert(regex("[a-z]{3}[0-9]{3}").run("foo123") == Right("foo123"))
    assert(regex("[a-z]{3}[0-9]{3}").run("foo") == Left(ValidationError("regex", Seq("[a-z]{3}[0-9]{3}"))))
  }

  test("lessThanEq") {
    assert(lessThanEq(3).run(3) == Right(3))
    assert(lessThanEq(3).run(4) == Left(ValidationError("lessThanEq", Seq("3"))))
  }

  test("greaterThanEq") {
    assert(greaterThanEq(3).run(3) == Right(3))
    assert(greaterThanEq(3).run(2) == Left(ValidationError("greaterThanEq", Seq("3"))))
  }

  test("lessThan") {
    assert(lessThan(3).run(2) == Right(2))
    assert(lessThan(3).run(3) == Left(ValidationError("lessThan", Seq("3"))))
    assert(lessThan(3).run(4) == Left(ValidationError("lessThan", Seq("3"))))
  }

  test("greaterThan") {
    assert(greaterThan(3).run(4) == Right(4))
    assert(greaterThan(3).run(3) == Left(ValidationError("greaterThan", Seq("3"))))
    assert(greaterThan(3).run(2) == Left(ValidationError("greaterThan", Seq("3"))))
  }

  test("between") {
    assert(between(1, 3).run(0) == Left(ValidationError("between", Seq("1", "3"))))
    assert(between(1, 3).run(1) == Right(1))
    assert(between(1, 3).run(2) == Right(2))
    assert(between(1, 3).run(3) == Right(3))
    assert(between(1, 3).run(4) == Left(ValidationError("between", Seq("1", "3"))))
  }

  test("equiv") {
    assert(equiv(3).run(3) == Right(3))
    assert(equiv(3).run(4) == Left(ValidationError("equiv", Seq("3"))))
  }

  test("same") {
    assert(same("A").run("A") == Right("A"))
    assert(same("B").run("A") == Left(ValidationError("same", Seq("B"))))
  }

  test("datetime ordering") {
    import java.time.LocalDateTime
    val now = LocalDateTime.parse("2016-01-01T00:00:00")

    assert(lessThanEq(now).run(now) == Right(now))
    assert(lessThanEq(now.plusDays(1)).run(now) == Right(now)) // now <= (now + 1)
    assert(lessThanEq(now.minusDays(1)).run(now) ==
      Left(ValidationError("lessThanEq", Seq("2015-12-31T00:00")))) // now <= (now - 1)

    assert(equiv(now).run(now) == Right(now))
  }
}
