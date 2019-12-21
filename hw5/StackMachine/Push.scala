package StackMachine


class Push(val x: Int) extends Command {
  def execute() {StackMachine.stack.push(x)}
}

object Push {
  def apply(x: Int) = new Push(x)
}