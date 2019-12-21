package expression
import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression{
  def execute(env: Environment): Value = {
    var args: List[Value] = List[Value]()
    flags.paramPassing match {
      case flags.BY_VALUE => args = operands.map(_.execute(env)); //println("BY_VALUE Detected: " + args)      
      case flags.BY_NAME => args = operands.map(new Thunk(_, env)); //println("BY_NAME Detected: " + args)
      case flags.BY_TEXT => args = operands.map(new Text(_)); //println("BY_TEXT Detected: " + args)
    }
    
    if (env.contains(operator) && env(operator).isInstanceOf[Closure]) env(operator).asInstanceOf[Closure].apply(args, env)
    else {
      // check for texts and thunks
      if (flags.paramPassing == flags.BY_NAME) alu.execute(operator, args.asInstanceOf[List[Thunk]].map(_.apply))
      else if (flags.paramPassing == flags.BY_TEXT) alu.execute(operator, args.asInstanceOf[List[Text]].map(_.body.execute(env)))
      else alu.execute(operator, args)
    }
    
  }
  override def toString = {
    var result = operator.toString + "("
    if (operands != Nil) {
      result = result + operands.head.toString
      for(operand <- operands.tail) {
        result = result + ", " + operand
      }
    }
    result = result + ")"
    result
  }
}