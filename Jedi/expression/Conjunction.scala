package expression
import value._
import context._

case class Conjunction(operands: List[Expression]) extends SpecialForm {

  override def toString = {
    var result = if (operands == Nil) "" else operands.head.toString
    for(operand <- operands.tail) result = result + " && " + operand.toString()
    result
  }
  // 1. Check if all are booleans by executing expressions
  // 2. op (&& op)*
  def execute(env: Environment): Value = {
    var result = true
    for (e <- operands if result != false) {
      e.execute(env) match{
        case Boole(value) => {
          if (value) result = false
        }
      }
    }
    Boole(result)
  }
}