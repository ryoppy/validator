package validator.playframework

import validator.ValidationValue

trait RequestSupport {
  implicit def requestSupport[A]: ValidationValue[play.api.mvc.Request[A]] = new ValidationValue[play.api.mvc.Request[A]] {
    def apply(request: play.api.mvc.Request[A]): Map[String, String] =
      ValidationValue.multiMap(playRequestToMap(request))
  }

  private def playRequestToMap(request: play.api.mvc.Request[_]): Map[String, Seq[String]] = {
    (request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString
  }
}
