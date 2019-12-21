/*
 * Problem 9: Monadic Bindings
 */
object monadic {
  def log(x: Double): Option[Double] = if (x <= 0) None else Some(math.log(x))
                                                  //> log: (x: Double)Option[Double]

  def sqrt(x: Double): Option[Double] = if (x < 0) None else Some(math.sqrt(x))
                                                  //> sqrt: (x: Double)Option[Double]
  
  def sqrtLog(x: Double): Option[Double] = {
  		x match {
  			case x if x >= 0 => val logVal = log(x); sqrt(logVal.get)
  			case default_ => None
  		}
  }                                               //> sqrtLog: (x: Double)Option[Double]
  
  
  sqrtLog(22)                                     //> res0: Option[Double] = Some(1.7581360736183977)
  sqrtLog(35)                                     //> res1: Option[Double] = Some(1.8855630621884312)
  sqrtLog(1)                                      //> res2: Option[Double] = Some(0.0)
  sqrt(0)                                         //> res3: Option[Double] = Some(0.0)
  sqrtLog(-1)                                     //> res4: Option[Double] = None
  sqrtLog(2367)                                   //> res5: Option[Double] = Some(2.7873605094271503)
  sqrtLog(-4352)                                  //> res6: Option[Double] = None
  
  
}