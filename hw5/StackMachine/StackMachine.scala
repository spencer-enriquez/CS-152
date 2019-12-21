package StackMachine

/*
 * Problem 8: Stack Machine -- INCOMPLETE
 * The Stack does not remove popped elements,causes out of bounds if try
 */
object StackMachine {
  val stack = new Stack[Int]()
  var program = List[Command]()
  
  def run() = {
    stack.clear
    for (cmd <- program)
      cmd.execute()
 }
}
