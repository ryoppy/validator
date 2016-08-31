package validator

trait ValidationValue[A] {
  def apply(x: A): Map[String, String]
}

object ValidationValue {
  implicit val forPlayFramework = new ValidationValue[Map[String, String]] {
    def apply(x: Map[String, String]): Map[String, String] = x
  }
}
