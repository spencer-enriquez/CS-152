package StackMachine

class Sum extends Command {
  def execute() {
    if (StackMachine.stack.top == None) println("Not enough elements in Stack to complete Sum.")
    else {
      val op1 = StackMachine.stack.top.get
      StackMachine.stack.pop()
      if (StackMachine.stack.top == None) {println("Not enough elements in Stack to complete Sum."); StackMachine.stack.push(op1)}
      else {
        val op2 = StackMachine.stack.top.get
        StackMachine.stack.pop()
        val sum = op1 + op2
        println("Sum = " + sum)
        val push = Push(sum)
        push.execute()
      }
    }
  }
}

object Sum {
  def apply() = new Sum
}