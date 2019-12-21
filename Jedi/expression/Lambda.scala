package expression
import context.Environment
import value._

case class Lambda(param: List[Identifier], body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = Closure(param, body, env)
  override def toString = {
    var result = ""
    for (p <- param) result += p
    result += " => " + body
    result
  }
}