package AccumulatorLab

class Mul(val x: Int) extends Command {
    def execute {Accumulator.register = Accumulator.register * x}
}

object Mul {
  def apply(x: Int) = new Mul(x)
}