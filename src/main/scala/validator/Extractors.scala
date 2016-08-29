package validator

trait Extractors {
  def string(name: String): Extractor[String] = Extractor[String](name, Right(_))

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

  def optional[A](a: Validation[A]): OptionExtractor[A] = OptionExtractor(a)

  def seq[A](a: Validation[A]): SeqExtractor[A] = SeqExtractor(a)

  private def numberFormat[A](x: String, name: String, f: String => A): Either[ValidationError, A] =
    try Right(f(x)) catch {
      case e: NumberFormatException => Left(ValidationError(name))
    }
}
