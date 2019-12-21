package StackMachine

class Pop extends Command{
  def execute() {StackMachine.stack.pop()}
}

object Pop {
  def apply() = new Pop
}