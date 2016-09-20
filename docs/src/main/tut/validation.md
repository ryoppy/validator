# Validation

* [Overview](validation.md#overview)
* [Types](validation.md#types)
* [Composing](validation.md#composing)

## Overview

A `Validation[A]` has type `A`.

```
val a: Validation[Int] = int("a")
```

and, it has `ValidationRule[A]`.

```
val between03: ValidationRule[Int] = between(0, 3)
val a: Validation[Int] = int("a") is between03
```

## Types

### Basic

- string
  - any strings.
- int
  - it should be able to `_.toInt`.
- double
- float
- long
- short
- byte
- boolean
  - "1" | "yes" | "true" are true.
  - "0" | "no" | "false" are false.
- char
- bigDecimal
- uuid
- ignored

### Date

- sqlDate
- jodaDateTime
- jodaLocalDate
- localDate
- localDateTime
- localTime

### Option

- optional

### Scala collections

- seq
- list
- set
- vector
- stream

## Composing

A product validation returns a product type represented as an `HList`.
To build product validation, use the `::` combinator.

```
val a: Validation[String] = string("a") is minLength(0)
val b: Validation[Int] = int("b")
val c: Validation[Double] = double("c")
val ab = a :: b
val abc: Validation[(String, Int, Double)] = (ab :: c).asTuple
abc.run(Map("a" -> "A", "b" -> "1", "c" -> "1.2")) == ValidationSuccess(("A", 1, 1.2d))
```

A coproduct validation returns a cpproduct type represented as an `Coproduct`.
To build coproduct validation, use the `:+:` combinator.

```
import shapeless.{:+:, Inl, CNil}

val a: Validation[String] = string("a") is minLength(0)
val b: Validation[Int] = int("b")
val c: Validation[Double] = double("c")
val ab = a :+: b
val abc: Validation[String :+: Int :+: Double :+: CNil] = (ab :+: c)
val result = abc.run(Map("a" -> "A", "b" -> "1", "c" -> "1.2"))
result == ValidationSuccess(Inl("A"))
result.value.flatMap(_.select[String]) == Some("A")
```