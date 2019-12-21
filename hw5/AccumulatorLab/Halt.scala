package AccumulatorLab

class Halt extends Command{
  def execute {Accumulator.halt = true}
}

object Halt {
  def apply() = new Halt
}