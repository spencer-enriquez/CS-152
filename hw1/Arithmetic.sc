/*
 * Mathematics Problem 3: Arithmetic Operations
 */
object arithmetic {
  
  def sqrt(n: Int): Option[Int] = {
     // = None if n < 0
     // = largest int m such that m * m <= n
     if (n < 0) None
     else {
     	var root = 0
     	while ((root + 1) * (root + 1) <= n) root = root + 1
     	Option(root)
     }
  }                                               //> sqrt: (n: Int)Option[Int]
  
  def log(n: Int): Option[Int] = {
     // = None if N <= 0
     // = largest m such that 2^m <= n
     if (n <= 0) None
     else {
     	var log = 0
     	while (scala.math.pow(2, log + 1) <= n) log = log + 1
     	Option(log)
     }
  }                                               //> log: (n: Int)Option[Int]
  
  def isPrime(n: Int): Option[Boolean] = {
    // = true if n has no divisors > 1
    if (n < 0) None
    else {
    		var prime = true
    		for (i <- 2 to n/2) if (n % i == 0) prime = false
    		Option(prime)
    }
  }                                               //> isPrime: (n: Int)Option[Boolean]
  
  def gcd(n: Int, m: Int): Option[Int] = {
    // = None if n or m < 0
    // = gcd(m, n) if n < m
    // = largest k dividing both n and m
    if (n < 0) None
     else {
     	var gcd = 1
     	for (i <- scala.math.min(n, m) until 1 by -1 if gcd == 1) if (n % i == 0 && m % i == 0) gcd = i
     	Option(gcd)
     }
  }                                               //> gcd: (n: Int, m: Int)Option[Int]
  
  def lcm(n: Int, m: Int): Option[Int] = {
    // = None if n < 0 or m < 0
    // = smallest k such that n a,d m divide k
    if (n < 0) None
     else {
     	var lcm = n * m
     	for (i <- 1 until n * m if lcm == n * m) if (i % n == 0 && i % m == 0) lcm = i
     	Option(lcm)
     }
  }                                               //> lcm: (n: Int, m: Int)Option[Int]
  
  def phi(n: Int): Option[Int] = {
    // = None if n < 0
    // = # of k <= n such that gcd(k, n) = 1
    if (n < 0) None
     else {
     	var phi = 0
     	for (i <- 1 until n) if (gcd(i, n) == Some(1)) phi = phi + 1
     	Option(phi)
     }
  }                                               //> phi: (n: Int)Option[Int]
  
  
  gcd(15, 12)                                     //> res0: Option[Int] = Some(3)
  lcm(15, 12)                                     //> res1: Option[Int] = Some(60)
  gcd(13, 12)                                     //> res2: Option[Int] = Some(1)
  gcd(-13, 12)                                    //> res3: Option[Int] = None
  phi(9)                                          //> res4: Option[Int] = Some(6)
  sqrt(49)                                        //> res5: Option[Int] = Some(7)
  sqrt(37)                                        //> res6: Option[Int] = Some(6)
  sqrt(35)                                        //> res7: Option[Int] = Some(5)
  log(64)                                         //> res8: Option[Int] = Some(6)
  log(130)                                        //> res9: Option[Int] = Some(7)
  log(9)                                          //> res10: Option[Int] = Some(3)
  log(0)                                          //> res11: Option[Int] = None
  isPrime(23)                                     //> res12: Option[Boolean] = Some(true)
  isPrime(59)                                     //> res13: Option[Boolean] = Some(true)
  isPrime(75)                                     //> res14: Option[Boolean] = Some(false)
}

/*
object testArith extends App {
  println("gcd(15, 12) = " + arithmetic.gcd(15, 12))
  println("lcm(15, 12) = " + arithmetic.lcm(15, 12))
  println("gcd(13, 12) = " + arithmetic.gcd(13, 12))
  println("gcd(-13, 12) = " + arithmetic.gcd(-13, 12))
  println("phi(9)= " + arithmetic.phi(9))
  println("sqrt(49) = " + arithmetic.sqrt(49))
  println("sqrt(37) = " + arithmetic.sqrt(37))
  println("sqrt(35) = " + arithmetic.sqrt(35))
  println("log(64) = " + arithmetic.log(64))
  println("log(130) = " + arithmetic.log(130))
  println("log(9) = " + arithmetic.log(9))
  println("log(0) = " + arithmetic.log(0))
  println("isPrime(23) = " + arithmetic.isPrime(23))
  println("isPrime(59) = " + arithmetic.isPrime(59))
  println("isPrime(75) = " + arithmetic.isPrime(75))
}
*/