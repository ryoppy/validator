package validator.play

import _root_.play.{api => playApi}
import validator.ValidationValue

trait RequestSupport {
  implicit def supportRequest[A]: ValidationValue[playApi.mvc.Request[A]] = new ValidationValue[playApi.mvc.Request[A]] {
    def apply(request: playApi.mvc.Request[A]): Map[String, String] =
      ValidationValue.multiMap(playRequestToMap(request))
  }

  private def playRequestToMap(request: playApi.mvc.Request[_]): Map[String, Seq[String]] = {
    (request.body match {
      case body: playApi.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: playApi.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: playApi.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString
  }
}
