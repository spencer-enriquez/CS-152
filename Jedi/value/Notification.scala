package value

class Notification(val message: String) extends Value {
  override def toString = message
}

object Notification {
  val OK =  Notification("OK")
  val DONE = Notification("Done")
  val UNSPECIFIED = Notification("Unspecified")
  
  def apply(message: String) = new Notification(message)
}