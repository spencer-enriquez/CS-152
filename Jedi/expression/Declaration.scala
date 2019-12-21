package expression
import context.Environment
import value._

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    env.put(id, exp.execute(env))
    Notification.OK
  }
  override def toString = "def " + id + " = " + exp
}