package expression

import context.Environment
import value._

case class Block(lines: List[Expression]) extends SpecialForm {
  override def toString = {
    var result = if (lines == Nil) "" else lines.head.toString + ";"
    for(line <- lines.tail) result = result + "\n" + lines.toString() + ";"
    result
  }
  
  def execute(env: Environment): Value = {
    if (lines == Nil) Notification.UNSPECIFIED
    else {
      val tempEnv = new Environment(env)
      for (i <- 0 to lines.length - 2) lines(i).execute(tempEnv)
      lines(lines.length - 1).execute(tempEnv)
    }
  }
}