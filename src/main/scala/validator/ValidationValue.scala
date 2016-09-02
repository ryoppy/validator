package validator

trait ValidationValue[A] {
  def apply(x: A): Map[String, String]
}

object ValidationValue {
  implicit val singleMap: ValidationValue[Map[String, String]] = new ValidationValue[Map[String, String]] {
    def apply(x: Map[String, String]): Map[String, String] = x
  }

  implicit val multiMap: ValidationValue[Map[String, Seq[String]]] = new ValidationValue[Map[String, Seq[String]]] {
    def apply(x: Map[String, Seq[String]]): Map[String, String] = fold(x)
  }

  implicit val multiMapOption: ValidationValue[Option[Map[String, Seq[String]]]] = new ValidationValue[Option[Map[String, Seq[String]]]] {
    def apply(x: Option[Map[String, Seq[String]]]): Map[String, String] = fold(x.getOrElse(Map.empty[String, Seq[String]]))
  }

  // NOTE: copied from playframework of Form class. https://github.com/playframework/playframework/blob/9f26118a52b8628fe911e6d5fe878d72fe0878b4/framework/src/play/src/main/scala/play/api/data/Form.scala#L92-L95
  private def fold(data: Map[String, Seq[String]]): Map[String, String] = {
    data.foldLeft(Map.empty[String, String]) {
      case (s, (key, values)) if key.endsWith("[]") => s ++ values.zipWithIndex.map { case (v, i) => (key.dropRight(2) + "[" + i + "]") -> v }
      case (s, (key, values)) => s + (key -> values.headOption.getOrElse(""))
    }
  }
}
