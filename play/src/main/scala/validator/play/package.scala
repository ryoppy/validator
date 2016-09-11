package validator

/**
 * `play.api.mvc.Request` support.
 * GET and POST parameters are subject to this.
  *
 * {{{
 * import validator.play.supportRequest
 *
 * def index = Action { req =>
 *   val v1 = int("id") is min(1)
 *   validate(req, v1)
 * }
 * }}}
 */
package object play extends RequestSupport {
}
