package expression
import context._
import value._

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    if (!env.contains(vbl) || !env(vbl).isInstanceOf[Variable]) throw new UndefinedException(vbl)
    env(vbl).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}