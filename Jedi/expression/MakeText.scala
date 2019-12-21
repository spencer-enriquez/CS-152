package expression
import context.Environment
import value._

// Parsed from delay
case class MakeText(body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = new Text(body)
  override def toString = "MakeText( " + body.toString + ")"
}