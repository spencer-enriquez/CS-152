package StackMachine

class Top extends Command {
    def execute()  {
      val result = StackMachine.stack.top
      if(result == None) println("Not enough elements in stack to get Top.")
      else println("Top = " + result.get)
    }
}

object Top {
  def apply() = new Top
}