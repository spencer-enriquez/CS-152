package value
import context._
import expression._

case class Closure(param: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value], callEnv: Environment): Value = {
    if (param.length != args.length) throw new JediException("# of parameters does not equal # of arguments")
    //val tempEnv = new Environment(defEnv)
    val tempEnv = if (flags.staticScope) new Environment(defEnv) else new Environment(callEnv)
    tempEnv.bulkPut(param, args)
    body.execute(tempEnv)
  }
  override def toString = body.toString
}