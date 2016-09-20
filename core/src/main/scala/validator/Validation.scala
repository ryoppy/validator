package validator

import shapeless.ops.adjoin.Adjoin
import shapeless.ops.hlist.Tupler
import shapeless.{:+:, CNil, Generic, HList, Inl, Inr}

trait Validation[A] {
  self =>

  def name: ValidationName

  def apply(x: String): ValidationResult[A]

  def run(xs: Map[String, String]): ValidationResult[A]

  final def addRule(rule: ValidationRule[A]): Validation[A] =
    new Validation[A] {
      def name = self.name
      def apply(x: String): ValidationResult[A] = self(x).flatMap(runRule)
      def run(xs: Map[String, String]): ValidationResult[A] = self.run(xs).flatMap(runRule)

      private def runRule(x: A): ValidationResult[A] =
        rule.run(x) match {
          case Right(r) => ValidationSuccess(r)
          case Left(e) => ValidationFailure.of(name -> e.toSeq)
        }
    }

  final def is(rule: ValidationRule[A]): Validation[A] = addRule(rule)

  final def and(rule: ValidationRule[A]): Validation[A] = addRule(rule)

  final def and(ruleName: RuleName)(f: A => Boolean): Validation[A] = addRule(ValidationRule(ruleName)(f))

  final def and(f: A => Boolean): Validation[A] = addRule(ValidationRule(self.name)(f))

  final def addRuleSelf(f: (Validation[A], A) => Validation[A]): Validation[A] = self.flatMap { x => f(self, x) }

  final def map[B](f: A => B): Validation[B] =
    new Validation[B] {
      def name = self.name
      def run(xs: Map[String, String]): ValidationResult[B] = self.run(xs).map(f)
      def apply(x: String): ValidationResult[B] = self(x).map(f)
    }

  final def flatMap[B](f: A => Validation[B]): Validation[B] =
    new Validation[B] {
      def name = self.name
      def run(xs: Map[String, String]): ValidationResult[B] = self.run(xs).flatMap(a => f(a).run(xs))
      def apply(x: String): ValidationResult[B] = self(x).flatMap(a => f(a).apply(x))
    }

  final def withFilter(f: A => Boolean): Validation[A] = filter(f)

  final def filter(f: A => Boolean): Validation[A] = filter(f, ValidationError(self.name))

  final def filter(f: A => Boolean, e: ValidationError): Validation[A] =
    self.transform { a =>
      if (f(a)) ValidationSuccess(a)
      else ValidationFailure.of(self.name -> Seq(e))
    }

  final def ::[B](next: Validation[B])(implicit pa: PairAdjoin[B, A]): Validation[pa.Out] =
    new Validation[pa.Out] {
      def name = next.name
      def run(xs: Map[String, String]): ValidationResult[pa.Out] = merge(self.run(xs), next.run(xs))
      def apply(x: String): ValidationResult[pa.Out] = merge(self(x), next(x))

      private def merge(v1: ValidationResult[A], v2: ValidationResult[B]): ValidationResult[pa.Out] = {
        (v1, v2) match {
          case (ValidationSuccess(x1), ValidationSuccess(x2)) =>
            ValidationSuccess(pa(x2, x1))
          case (ValidationSuccess(x1), ValidationFailure(e2)) =>
            ValidationFailure(e2)
          case (ValidationFailure(e1), ValidationSuccess(x2)) =>
            ValidationFailure(e1)
          case (ValidationFailure(e1), ValidationFailure(e2)) =>
            ValidationFailure(e1 ++ e2)
        }
      }
    }

  final def :+:[B](that: Validation[B])(implicit a: Adjoin[B :+: A :+: CNil]): Validation[a.Out] =
    that.map(x => a(Inl[B, A :+: CNil](x))) |
    self.map(x => a(Inr[B, A :+: CNil](Inl[A, CNil](x))))

  final def orElse[B >: A](that: Validation[B]): Validation[B] =
    new Validation[B] {
      def name = that.name
      def run(xs: Map[String, String]): ValidationResult[B] = merge(self.run(xs), that.run(xs))
      def apply(x: String): ValidationResult[B] = merge(self(x), that(x))

      private def merge(r1: ValidationResult[A], r2: ValidationResult[B]): ValidationResult[B] = {
        (r1, r2) match {
          case (ValidationSuccess(x1), _) =>
            ValidationSuccess(x1)
          case (_, ValidationSuccess(x2)) =>
            ValidationSuccess(x2)
          case (ValidationFailure(e1), ValidationFailure(e2)) =>
            ValidationFailure(e1 ++ e2)
        }
      }
    }

  final def |[B >: A](that: Validation[B]): Validation[B] = orElse(that)

  final def sameValue(that: Validation[A]): Validation[A] = self.flatMap { a => that is same(a) }

  final def transform[B](f: A => ValidationResult[B]): Validation[B] =
    new Validation[B] {
      def name = self.name
      def run(xs: Map[String, String]): ValidationResult[B] = self.run(xs).flatMap(f)
      def apply(x: String): ValidationResult[B] = self(x).flatMap(f)
    }

  final def rescue[B >: A](pf: PartialFunction[ValidationFailure[A], ValidationResult[B]]): Validation[B] =
    new Validation[B] {
      def name = self.name

      def run(xs: Map[String, String]): ValidationResult[B] =
        self.run(xs).fold(
          { e => PartialFunction.condOpt(ValidationFailure[A](e))(pf).getOrElse(ValidationFailure[B](e)) }, 
          { x => ValidationSuccess(x) }
        )

      def apply(x: String): ValidationResult[B] =
        self(x).fold(
          { e => PartialFunction.condOpt(ValidationFailure[A](e))(pf).getOrElse(ValidationFailure[B](e)) }, 
          { x => ValidationSuccess(x) }
        )
    }
}

object Validation extends Validation22 {

  final implicit class HListValidationOps[L <: HList](val self: Validation[L]) extends AnyVal {
    def as[A](implicit gen: Generic.Aux[A, L]): Validation[A] = self.map(gen.from)

    def asTuple(implicit tupler: Tupler[L]): Validation[tupler.Out] = self.map(tupler(_))
  }
}
