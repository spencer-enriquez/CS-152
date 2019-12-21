package value
import expression.Literal

case class Pair(first: Value, second: Value) extends Literal with Equals {
  def f: Value = first
  def s: Value = second
  override def toString = "(" + first + ", " + second + ")"
  override def canEqual(other: Any) =  other.isInstanceOf[Pair]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Pair => this.canEqual(other) && (other.first == this.first) && 
                           (other.second == this.second)
       case _ => false
    }
  override def hashCode = this.toString.##
}