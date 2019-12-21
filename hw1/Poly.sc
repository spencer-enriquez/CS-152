import scala.math._

/*object Mathematics {

	val p = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)
  
  println("eval(6, p) = " + poly.eval(6, p))
  println("eval(2, p) = " + poly.eval(2, p))
  println("eval(-5, p) = " + poly.eval(-5, p))
  
  println("roots(p) = " + poly.roots(p))
  
  println("deriv(p) = " + poly.deriv(p))
  println("deriv2(p) = " + poly.deriv(poly.deriv(p)))
  
}
*/

/*
 * Mathematics Problem 1: Polynomials
 */
object poly {
  
  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    // = None if p has no real roots
    // = Some((r1, r2)) where p(r1) == p(r2) == 0
    val sqrtQuad = scala.math.sqrt(p._2 * p._2 - 4 * p._1 * p._3)
    var r1 = Double.NaN
    var r2 = Double.NaN
    if (!sqrtQuad.isNaN) {r1 = (((-1 * p._2) + (sqrtQuad)) / (2 * p._1)); r2 = (((-1 * p._2) - (sqrtQuad)) / (2 * p._1))}
    Option (r1, r2)
  }
  
  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
    // = derivative of p (which should be degree 1
    (0, p._1 * 2, p._2)
    
  def eval(a: Double, p: (Double, Double, Double)): Double =
    // = p(a)
    p._1 * (a * a) + p._2 * a + p._3
}

object PolyTest extends App {
  val p = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)
  
  println("eval(6, p) = " + poly.eval(6, p))
  println("eval(2, p) = " + poly.eval(2, p))
  println("eval(-5, p) = " + poly.eval(-5, p))
  
  println("roots(p) = " + poly.roots(p))
  
  println("deriv(p) = " + poly.deriv(p))
  println("deriv2(p) = " + poly.deriv(poly.deriv(p)))
  
}