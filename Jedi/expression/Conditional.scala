package expression
import value._
import context._

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm {

  override def toString = {
    var result = "if (" + condition + ") " + consequent
    if (alternative != null) result = result + " else " + alternative
    result
  }

  // etc.
  def execute(env: Environment): Value = {
    condition.execute(env) match{
      case Boole(value) => {
        if (value) consequent.execute(env)
        else if (alternative != null) alternative.execute(env)
        else Notification.UNSPECIFIED
      }
      case _ => throw new TypeException("if condition must be type Boole")
    }
  }
}