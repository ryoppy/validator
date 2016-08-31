package validator

import scala.annotation.tailrec

final case class Extractor[A](name: ValidationName, extract: String => Either[ValidationError, A]) extends Validation[A] {
  def apply(value: String): ValidationResult[A] = {
    extract(value) match {
      case Right(x) => ValidationSuccess(x)
      case Left(e) => ValidationFailure(name -> Seq(e))
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

final case class SeqExtractor[A](a: Validation[A]) extends Validation[Seq[A]] {
  def name = a.name

  def indexes(key: String, data: Map[String, String]): Seq[Int] = {
    val KeyPattern = ("^" + java.util.regex.Pattern.quote(key) + """\[(\d+)\].*$""").r
    data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
  }

  override def findValue(params: Map[String, String]): Seq[String] = {
    indexes(name, params).flatMap { i => params.get(s"$name[$i]") }
  }

  override def run(params: Map[String, String]): ValidationResult[Seq[A]] = {
    @tailrec
    def f(xs: Seq[ValidationResult[A]], acc: ValidationResult[Seq[A]]): ValidationResult[Seq[A]] = {
      xs match {
        case x +: t =>
          f(t, for { xv <- x; av <- acc } yield xv +: av)
        case Nil =>
          acc
      }
    }
    f(findValue(params).map(a.apply), ValidationSuccess(Nil)).map(_.reverse)
  }
  
  def apply(value: String): ValidationResult[Seq[A]] = {
    a.apply(value).map(Seq(_))
  }
}
