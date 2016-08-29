package validator

import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

trait Validation[A] {
  self =>

  def name: String

  def run(params: String): ValidationResult[A]

  protected def findValue(params: Map[String, String]): Seq[String] = params.get(name).toSeq

  def execute(params: Map[String, String]): ValidationResult[A] = {
    findValue(params) match {
      case Nil =>
        ValidationFailure(name -> Seq(ValidationError("required")))
      case value +: _ =>
        run(value)
    }
  }

  def addRule(rule: ValidationRule[A]): Validation[A] =
    new Validation[A] {
      def name = self.name

      override def run(params: String): ValidationResult[A] = {
        self.run(params).flatMap { x =>
          rule.run(x) match {
            case Right(r) => ValidationSuccess(r)
            case Left(e) => ValidationFailure(name -> Seq(e))
          }
        }
      }
    }

  def is(rule: ValidationRule[A]): Validation[A] = addRule(rule)

  def and(rule: ValidationRule[A]): Validation[A] = addRule(rule)

  def transform[B](f: A => ValidationResult[B]): Validation[B] = ???

  def map[B](f: A => B): Validation[B] =
    new Validation[B] {
      def name = self.name

      override protected def findValue(params: Map[String, String]): Seq[String] = self.findValue(params)

      override def execute(params: Map[String, String]): ValidationResult[B] = self.execute(params).map(f)

      def run(params: String): ValidationResult[B] =
        self.run(params).map(f)
    }

  def flatMap[B](f: A => Validation[B]): Validation[B] =
    new Validation[B] {
      def name = self.name

      override protected def findValue(params: Map[String, String]): Seq[String] = self.findValue(params)

      override def execute(params: Map[String, String]): ValidationResult[B] =
        self.execute(params).flatMap(a => f(a).execute(params))

      def run(params: String): ValidationResult[B] =
        self.run(params).flatMap(a => f(a).run(params))
    }

  def withFilter(p: A => Boolean): Validation[A] =
    new Validation[A] {
      def name = self.name

      override protected def findValue(params: Map[String, String]): Seq[String] = self.findValue(params)

      override def execute(params: Map[String, String]): ValidationResult[A] =
        self.addRule(ValidationRule(name)(p)).execute(params)

      def run(params: String): ValidationResult[A] =
        self.addRule(ValidationRule(name)(p)).run(params)
    }

  def ::[B](x: Validation[B])(implicit pa: PairAdjoin[B, A]): Validation[pa.Out] =
    new Validation[pa.Out] {
      def name = x.name

      override protected def findValue(params: Map[String, String]): Seq[String] = x.findValue(params)

      override def execute(params: Map[String, String]): ValidationResult[pa.Out] =
        merge(self.execute(params), x.execute(params))

      def run(params: String): ValidationResult[pa.Out] =
        merge(self.run(params), x.run(params))

      private def merge(v1: ValidationResult[A], v2: ValidationResult[B]): ValidationResult[pa.Out] = {
        (v1, v2) match {
          case (ValidationSuccess(x1), ValidationSuccess(x2)) =>
            ValidationSuccess(pa(x2, x1))
          case (ValidationSuccess(x1), ValidationFailure(e2)) =>
            ValidationFailure(e2)
          case (ValidationFailure(e1), ValidationSuccess(x2)) =>
            ValidationFailure(e1)
          case (ValidationFailure(e1), ValidationFailure(e2)) =>
            ValidationFailure(e1, e2)
        }
      }
    }
}

object Validation {

  final implicit class HListValidationOps[L <: HList](val self: Validation[L]) extends AnyVal {
    def as[A](implicit gen: Generic.Aux[A, L]): Validation[A] = self.map(gen.from)

    def asTuple(implicit tupler: Tupler[L]): Validation[tupler.Out] = self.map(tupler(_))
  }

}
