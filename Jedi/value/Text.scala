package value
import expression.Expression

case class Text(val body: Expression) extends Value {
  override def toString = "Text( " + body.toString + ")"
}