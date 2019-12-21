package expression
import value.Value
import context.Environment

trait Literal extends Expression with Value{
  // identity function
  def execute(env: Environment): Value = this
}