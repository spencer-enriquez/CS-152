package context

object flags {
  val BY_VALUE: Int = 0
  val BY_NAME: Int = 1
  val BY_TEXT: Int = 2
  var staticScope = true
  var paramPassing = BY_VALUE
}