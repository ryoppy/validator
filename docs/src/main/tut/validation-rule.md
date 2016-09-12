# ValidationRule

* [Overview](validation-rule.md#overview)
* [Types](validation-rule.md#types)
* [Composing](validation-rule.md#composing)
* [Custom Rules](validation-rule.md#custom-rules)

## Overview

A `ValidationRule[A]` has type `A`.

```
val a: ValidationRule[Int] = min(0)
```

## Types

### String

- minLength
- maxLength
- exactLength
- email
- ip
- ip4
- ip6
- url
- regex

### Numeric | DateTime

- lessThan
- greaterThan
- lessThanEq
- greaterThanEq
- between
- equiv

### Others

- same

## Composing

use the `and` operator.

```
val rule1: ValidationRule[String] = minLength(1) and maxLength(10)
val rule2: ValidationRule[String] = rule1 and equal("A")
```

## Custom Rules

example1

```
val creditCard: ValidationRule[String] = ValidationRule[String]("creditCart") { x => isCreditCard(x) }
```

example2

```
def uniqueEmail(email: String): ValidationRule[String] = {
  ValidationRule("uniqueEmail") { y =>
    val result: Option[String] = sqls"SELECT email FROM users WHERE email = ${email} LIMIT 1".single.apply()
    result.isDefined
  }
}

val emailValidation: Validation[String] = {
  string("email") flatMapWith { (v, x) => v is email and unique(x) }
}
```

