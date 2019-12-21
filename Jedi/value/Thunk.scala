package value
import expression.Expression
import expression._
import context.Environment

class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv) {
  var cache: Value = null
  def apply(): Value = {
    // set defEnv as calling environment
    if (cache == null) cache = super.apply(Nil, defEnv)
    cache
  }
  override def toString = "Thunk( " + body.toString + ")"
}