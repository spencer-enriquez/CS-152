package AccumulatorLab

class Goto(val arg: Int) extends Command {
  def execute {Accumulator.IP = arg - 1}
}

object Goto {
  def apply(arg: Int) = new Goto(arg)
}