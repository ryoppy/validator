package validator

import java.util.{TimeZone, UUID}

import scala.collection.generic.CanBuildFrom
import scala.util.control.NonFatal

trait Extractors {
  def string(name: String): Extractor[String] = Extractor[String](name, x => Right(x.trim))

  def int(name: String): Extractor[Int] =
    Extractor[Int](name, x => numberFormat(x, "int", _.toInt))

  def double(name: String): Extractor[Double] =
    Extractor[Double](name, x => numberFormat(x, "double", _.toDouble))

  def float(name: String): Extractor[Float] =
    Extractor[Float](name, x => numberFormat(x, "float", _.toFloat))

  def long(name: String): Extractor[Long] =
    Extractor[Long](name, x => numberFormat(x, "long", _.toLong))

  def short(name: String): Extractor[Short] =
    Extractor[Short](name, x => numberFormat(x, "short", _.toShort))

  def byte(name: String): Extractor[Byte] =
    Extractor[Byte](name, x => numberFormat(x, "byte", _.toByte))

  def boolean(name: String): Extractor[Boolean] =
    Extractor[Boolean](name, x => isBoolean(x.trim) match {
      case Some(true) => Right(true)
      case Some(false) => Right(false)
      case None => Left(ValidationError("boolean"))
    })

  private def isBoolean(x: String): Option[Boolean] = x.toLowerCase match {
    case "true" | "1" | "ok" => Some(true)
    case "false" | "0" | "ng" => Some(false)
    case _ => None
  }

  def char(name: String): Extractor[Char] =
    Extractor[Char](name, x =>
      if (name.length == 1) Right(x.charAt(0)) else Left(ValidationError("char"))
    )

  def bigDecimal(name: String): Extractor[BigDecimal] =
    Extractor[BigDecimal](name, x =>
      tryCatch(x, "bigDecimal", y => BigDecimal(y))
    )

  private def toDateTime(x: String, pattern: String, timeZone: TimeZone): org.joda.time.DateTime = {
    val jodaTimeZone = org.joda.time.DateTimeZone.forTimeZone(timeZone)
    val formatter = org.joda.time.format.DateTimeFormat.forPattern(pattern).withZone(jodaTimeZone)
    formatter.parseDateTime(x)
  }

  def sqlDate(name: String, timeZone: TimeZone = TimeZone.getDefault): Extractor[java.sql.Date] = {
    Extractor[java.sql.Date](name, x =>
      tryCatch(x, "sqlDate", y => new java.sql.Date(toDateTime(y, "yyyy-MM-dd", timeZone).toDate.getTime))
    )
  }

  def jodaDateTime(name: String, timeZone: TimeZone = TimeZone.getDefault): Extractor[org.joda.time.DateTime] = {
    Extractor[org.joda.time.DateTime](name, x =>
      tryCatch(x, "jodaDateTime", y => toDateTime(y, "yyyy-MM-dd", timeZone))
    )
  }

  def jodaLocalDate(name: String): Extractor[org.joda.time.LocalDate] = {
    val formatter = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd")
    Extractor[org.joda.time.LocalDate](name, x =>
      tryCatch(x, "jodaLocalDate", y => org.joda.time.LocalDate.parse(y, formatter))
    )
  }

  def localDate(name: String): Extractor[java.time.LocalDate] = {
    val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")
    Extractor[java.time.LocalDate](name, x =>
      tryCatch(x, "localDate", y => java.time.LocalDate.parse(y, formatter))
    )
  }

  def localDateTime(name: String, zoneId: java.time.ZoneId = java.time.ZoneId.systemDefault()): Extractor[java.time.LocalDateTime] = {
    val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(zoneId)
    Extractor[java.time.LocalDateTime](name, x =>
      tryCatch(x, "localDateTime", y => java.time.LocalDateTime.parse(y, formatter))
    )
  }

  def localTime(name: String): Extractor[java.time.LocalTime] = {
    val formatter = java.time.format.DateTimeFormatter.ofPattern("HH:mm:ss")
    Extractor[java.time.LocalTime](name, x =>
      tryCatch(x, "localDateTime", y => java.time.LocalTime.parse(y, formatter))
    )
  }

  def uuid(name: String): Extractor[UUID] = {
    Extractor[UUID](name, x =>
      tryCatch(x, "uuid", y => UUID.fromString(y))
    )
  }

  def ignored[A](name: String, a: A): Extractor[A] = {
    Extractor[A](name, _ => Right(a))
  }

  def optional[A](a: Validation[A]): OptionExtractor[A] = OptionExtractor(a)

  def seq[A](a: Validation[A])(implicit cbf: CanBuildFrom[Nothing, A, Seq[A]]): SeqExtractor[Seq, A] = SeqExtractor[Seq, A](a, cbf)

  def list[A](a: Validation[A])(implicit cbf: CanBuildFrom[Nothing, A, List[A]]): SeqExtractor[List, A] = SeqExtractor[List, A](a, cbf)

  def set[A](a: Validation[A])(implicit cbf: CanBuildFrom[Nothing, A, Set[A]]): SeqExtractor[Set, A] = SeqExtractor[Set, A](a, cbf)

  def vector[A](a: Validation[A])(implicit cbf: CanBuildFrom[Nothing, A, Vector[A]]): SeqExtractor[Vector, A] = SeqExtractor[Vector, A](a, cbf)

  def stream[A](a: Validation[A])(implicit cbf: CanBuildFrom[Nothing, A, Stream[A]]): SeqExtractor[Stream, A] = SeqExtractor[Stream, A](a, cbf)

  private def numberFormat[A](x: String, name: String, f: String => A): Either[ValidationError, A] =
    try Right(f(x.trim)) catch {
      case e: NumberFormatException => Left(ValidationError(name))
    }

  private def tryCatch[A](x: String, name: String, f: String => A): Either[ValidationError, A] =
    try Right(f(x.trim)) catch {
      case NonFatal(_) => Left(ValidationError(name))
    }
}
