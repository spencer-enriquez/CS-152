package StackMachine

class Times extends Command{
    def execute() {
    val op1 = StackMachine.stack.top
    StackMachine.stack.pop()
    val op2 = StackMachine.stack.top
    StackMachine.stack.pop()
    if (op1 == None || op2 == None) {
      println("Not enough elements in Stack to create Times.")
      if (op1 != None) StackMachine.stack.push(op1.get)
    }
    else {
      val product = op1.get * op2.get
      StackMachine.stack.push(product)
    }
  }
}

object Times {
  def apply() = new Times
}