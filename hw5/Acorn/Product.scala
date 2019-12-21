package Acorn

class Product(private var op1: Expression, private var op2: Expression) extends Expression{
  def execute(): Double = op1.execute * op2.execute
  override def toString(): String = "(* " + op1 + " " + op2 + ")"
}

object Product {
  def apply(operand1: Expression, operand2: Expression) =
      new Product(operand1, operand2)
}