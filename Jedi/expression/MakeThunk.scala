package expression
import context.Environment
import value._

// Parsed from freeze
case class MakeThunk(body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = new Thunk(body, env)
  override def toString = "MakeThunk( " + body.toString + ")"
}