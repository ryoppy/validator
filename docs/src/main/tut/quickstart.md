## Quick start

validator is published to [Maven Central][maven-central] and cross-built for Scala 2.10 and 2.11, so
you can just add the following to your build:

```scala
val validatorVersion = "0.5.1"

libraryDependencies ++= Seq(
  "io.validator" %% "validator-core",
  "io.validator" %% "validator-generic",
  "io.validator" %% "validator-parser"
).map(_ % validatorVersion)
```

Then type `sbt console` to start a REPL and then paste the following (this will also work from the
root directory of this repository):

```tut:book
import io.validator._
```

No boilerplate, no runtime reflection.
