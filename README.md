# Validator

[![Travis CI](https://travis-ci.org/ryoppy/validator.svg?branch=master)](https://travis-ci.org/ryoppy/validator)

Validator is a Validation library for Scala.

PlayのFormには苦労させられたので、型安全で合成可能で22個の制限がなく、柔軟に書けるものがあると幸せになれるかと思い作りました。

## Example

```scala
import validator._

case class Foo(a: String, b: Int)

val v1: Validation[Foo] = Validation(
  string("a") is minLength(1),
  int("b") is min(1) and max(3)
).as[Foo]

val result = validate(Map("a" -> "A", "b" -> "1"), v1)
assert(result == ValidationSuccess(Foo("A", 1)))

val result2 = validate(Map("a" -> "A", "b" -> "0"), v1)
assert(result2 == ValidationFailure("b" -> Seq(ValidationError("min", Seq("1")))))
```

compsing

```scala
val v1: Validation[String] = string("a") is minLength(1)
val v2: Validation[Int] = int("a") is min(1)
val v3: Validation[Foo] = (v1 :: v2).as[Foo]
val v4: Validation[(String, Int)] = (v1 :: v2).asTuple
```

for-comprehension

```scala
val v1: Validation[Foo] = for {
  a <- string("a") is minLength(1)
  b <- int("a") is min(1)
} yield Foo(a, b)
```

[more examples](src/test/scala/validator/ExampleSpec.scala).

[tests](src/test/scala/validator).

## QuickStart

TOOD: mavenに登録する。

```

```


## License

MIT