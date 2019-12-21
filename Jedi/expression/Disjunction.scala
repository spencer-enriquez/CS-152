package expression
import value._
import context._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {

  override def toString = {
    var result = if (operands == Nil) "" else operands.head.toString
    for(operand <- operands.tail) result = result + " || " + operand.toString()
    result
  }
  
  def execute(env: Environment): Value = {
    var result = false
    for (e <- operands if result != true) {
      e.execute(env) match{
        case Boole(value) => {
          if (value) result = true
        }
      }
    }
    Boole(result)
  }
}
  // etc.