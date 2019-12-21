import scala.math._


/*
 * Problem 2: Vectror Opertations
 */
object vector {
  
  def sum(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) =
    // v1 + v2
    (v1._1 + v2._1, v1._2 + v2._2, v1._3 + v2._3) //> sum: (v1: (Double, Double, Double), v2: (Double, Double, Double))(Double, Do
                                                  //| uble, Double)
    
  def mul(a: Double,v: (Double, Double, Double)): (Double, Double, Double) =
    // = a * v
    (a * v._1, a * v._2, a * v._3)                //> mul: (a: Double, v: (Double, Double, Double))(Double, Double, Double)
      
  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
    //= v1 * v2
    v1._1 * v2._1 + v1._2 * v2._2 + v1._3 * v2._3 //> dot: (v1: (Double, Double, Double), v2: (Double, Double, Double))Double
  
  def length(v: (Double, Double, Double)): Double =
    // = |v|
    scala.math.sqrt(v._1 * v._1 + v._2 * v._2 + v._3 * v._3)
                                                  //> length: (v: (Double, Double, Double))Double
    
  
  def theta(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double = {
    // = angle (in radians) between v1 and v2
    val product = dot(v1, v2) / (length(v1) * length(v2))
    scala.math.acos(product)
  }                                               //> theta: (v1: (Double, Double, Double), v2: (Double, Double, Double))Double
  
  
  val v1 = (2.0, 2.0, 2.0)                        //> v1  : (Double, Double, Double) = (2.0,2.0,2.0)
  val v2 = (1.0, 0.0, 0.0)                        //> v2  : (Double, Double, Double) = (1.0,0.0,0.0)
  val v3 = (0.0, 1.0, 0.0)                        //> v3  : (Double, Double, Double) = (0.0,1.0,0.0)
  
  sum(v3, v2)                                     //> res0: (Double, Double, Double) = (1.0,1.0,0.0)
  mul(3, v1)                                      //> res1: (Double, Double, Double) = (6.0,6.0,6.0)
  
  dot(v1, v2)                                     //> res2: Double = 2.0
  dot(v2, v3)                                     //> res3: Double = 0.0
  dot(v1, v1)                                     //> res4: Double = 12.0
  
  length(v1)                                      //> res5: Double = 3.4641016151377544
  length(v2)                                      //> res6: Double = 1.0
  
  theta(v1, v2)                                   //> res7: Double = 0.9553166181245092
  theta(v3, v2)                                   //> res8: Double = 1.5707963267948966
}

/*
object VectorTest extends App {
  val v1 = (2.0, 2.0, 2.0)
  val v2 = (1.0, 0.0, 0.0)
  val v3 = (0.0, 1.0, 0.0)
  
  println("sum(v3, v2) = " + vector.sum(v3, v2))
  println("mul(3, v1) = " + vector.mul(3, v1))
  
  println("dot(v1, v2) = " + vector.dot(v1, v2))
  println("dot(v2, v3) = " + vector.dot(v2, v3))
  println("dot(v1, v1) = " + vector.dot(v1, v1))
  
  println("length(v1) = " + vector.length(v1))
  println("length(v2) = " + vector.length(v2))
  
  println("theta(v1, v2) = " + vector.theta(v1, v2))
  println("theta(v3, v2) = " + vector.theta(v3, v2))
  println("pi/2 = " + Math.PI/2)
}
*/