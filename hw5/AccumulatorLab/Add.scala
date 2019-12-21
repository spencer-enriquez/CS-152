package AccumulatorLab

class Add(val x: Int) extends Command {
    def execute {Accumulator.register = Accumulator.register + x}
}

object Add {
  def apply(x: Int) = new Add(x)
}