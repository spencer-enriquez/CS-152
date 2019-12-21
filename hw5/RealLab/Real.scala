package RealLab

class Real(val scalaValue: Double) extends Ordered[Real] with Equals {  
  def +(other: Real) = Real(scalaValue + other.scalaValue)
  def *(other: Real) = Real(scalaValue * other.scalaValue)
  def -(other: Real) = Real(scalaValue - other.scalaValue)
  def /(other: Real) = Real(scalaValue / other.scalaValue)
  def unary_-() = Real(-1  * scalaValue)
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Real]
  def compare(that: Real): Int = {
    if (scalaValue - that.scalaValue == 0) 0
    else if (scalaValue - that.scalaValue < 0) -1
    else 1
  }
  override def hashCode = this.toString().##
  override def toString = scalaValue.toString()
  override def equals(other: Any): Boolean =
    other match {
       case other: Real => this.canEqual(other) &&
                           this.## == other.## &&
                           (other.scalaValue == this.scalaValue)
       case _ => false
    }
}

object Real {
  def apply(x: Double) = new Real(x)
}
/*
trait Arithmetic {
  def +(num: Arithmetic)
  def *(num: Arithmetic)
  def -(num: Arithmetic)
  def /(num: Arithmetic)
  def unary_-(num: Arithmetic)
}

object RealTest extends App {
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
}
*/