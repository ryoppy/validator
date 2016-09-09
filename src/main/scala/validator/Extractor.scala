package validator

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

final case class Extractor[A](name: ValidationName, extract: String => Either[ValidationError, A]) extends Validation[A] {
  def apply(value: String): ValidationResult[A] = {
    extract(value) match {
      case Right(x) => ValidationSuccess(x)
      case Left(e) => ValidationFailure.of(name -> Seq(e))
    }
  }
}

final case class OptionExtractor[A](a: Validation[A]) extends Validation[Option[A]] {
  def name = a.name

  override def run(params: Map[String, String]): ValidationResult[Option[A]] = {
    findValue(params) match {
      case Nil =>
        ValidationSuccess(None)
      case value +: _ =>
        apply(value)
    }
  }
  
  def apply(value: String): ValidationResult[Option[A]] = {
    a(value).map(Some(_))
  }
}

final case class SeqExtractor[C[_], A](a: Validation[A],
                                       cbf: CanBuildFrom[Nothing, A, C[A]]) extends Validation[C[A]] {
  def name = a.name

  def indexes(key: String, data: Map[String, String]): Seq[Int] = {
    val KeyPattern = ("^" + java.util.regex.Pattern.quote(key) + """\[(\d+)\].*$""").r
    data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
  }

  override def findValue(params: Map[String, String]): Seq[String] = {
    indexes(name, params).flatMap { i => params.get(s"$name[$i]") }
  }

  override def run(params: Map[String, String]): ValidationResult[C[A]] = {
    val xs: Seq[ValidationResult[A]] = findValue(params).map(a.apply)

    @tailrec
    def f(ys: Seq[ValidationResult[A]],
          i: Int,
          su: Seq[A],
          fa: Seq[ValidationResult.Error]): (Seq[A], Seq[ValidationResult.Error]) = {
      ys match {
        case ValidationSuccess(h) +: t =>
          f(t, i + 1, h +: su, fa)
        case ValidationFailure((name, e) +: _) +: t =>
          f(t, i + 1, su, (s"$name[$i]", e) +: fa)
        case Nil =>
          (su.reverse, fa.reverse)
      }
    }
    f(xs, 0, Nil, Nil) match {
      case (su, Nil) =>
        ValidationSuccess((cbf() ++= su).result)
      case (_, Seq(fa @ _*)) =>
        ValidationFailure(fa)
    }
  }
  
  def apply(value: String): ValidationResult[C[A]] = {
    val builder = cbf.apply
    a.apply(value).map { x =>
      builder += x
      builder.result
    }
  }
}
