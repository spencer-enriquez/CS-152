package Acorn

class Number(private var value: Double = 0.0) extends Expression {
  def num = value
  def execute(): Double = num
  override def toString(): String = num + ""
}

object Number {
  def apply(num: Double) =
      new Number(num)
}