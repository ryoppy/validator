# Validator

[日本語](README-ja.md)

## This project is alpha release. Do not use production environments.

[![Travis CI](https://travis-ci.org/ryoppy/validator.svg?branch=master)](https://travis-ci.org/ryoppy/validator)

Validator is a Validation library for Scala.

## Install

You can just add the following to your build.

```
libraryDependencies += "com.github.ryoppy" %% "validator-core" % "0.0.1"
```

## Example

```scala
import validator._

case class Foo(a: String, b: Int)

val v1: Validation[Foo] = Validation(
  string("a") is minLength(1) and maxLength(10),
  int("b") is between(1, 3)
).as[Foo]

val result = validate(Map("a" -> "A", "b" -> "1"), v1)
assert(result == ValidationSuccess(Foo("A", 1)))

val result2 = validate(Map("a" -> "A", "b" -> "0"), v1)
assert(result2 == ValidationFailure("b" -> Seq(ValidationError("between", Seq("1", "3")))))
```

composing

```scala
val v1: Validation[String] = string("a") is minLength(1)
val v2: Validation[Int] = int("a") is lessThan(1)
val v3: Validation[Foo] = (v1 :: v2).as[Foo]
val v4: Validation[(String, Int)] = (v1 :: v2).asTuple
```

for-comprehension

```scala
val v1: Validation[Foo] = for {
  a <- string("a") is minLength(1)
  b <- int("b") is lessThan(1)
} yield Foo(a, b)
```

[more examples](core/src/test/scala/validator/ExampleSpec.scala).

[tests](core/src/test/scala/validator).

## Playframework Example

```
// POST
validate(req.body.asFormUrlEncoded, v1)

// GET
validate(req.queryString, v1)
```

validate method is able to received `Map[String, Seq[String]]` or [something](core/src/main/scala/validator/ValidationValue.scala).

## Why?

A Playframework Form class is not composable, and 22 limit. so I made this library.

## License

MIT