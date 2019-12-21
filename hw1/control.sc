object Sequence_Control {

	// Problem 1: Number Kingdoms
  def kingdom(n: Int): Int =
  {
  		var kingdom = 4;
  		if (n > 10) {
  			if (n % 2 == 0) {
  				kingdom = if (n % 100 == 0) 1 else 2
  			}
  			else kingdom = 4
  		}
  		else if (n <= 10) return 3
  		else kingdom = 4
  		kingdom
  }                                               //> kingdom: (n: Int)Int
  
  kingdom(55)                                     //> res0: Int = 4
  kingdom(200)                                    //> res1: Int = 1
  kingdom(8)                                      //> res2: Int = 3
  kingdom(156)                                    //> res3: Int = 2
  kingdom(1800)                                   //> res4: Int = 1
  kingdom(887)                                    //> res5: Int = 4
  kingdom(-5)                                     //> res6: Int = 3
  kingdom(668)                                    //> res7: Int = 2
  
  
  // Problem 2: Number Orders
  def order(n: Int): Int =
  {
  		var family = 2;
  		var Class = 50;
  		var genus = 4;
  		family = if (n % 3 == 0) 1 else 2
  		Class = if (n == 50) 3 else 4
  		genus = if (n % 7 == 0) 5 else 6
  		if (n <= 0) 0 else family * Class + genus
  }                                               //> order: (n: Int)Int
  
  order(7)                                        //> res8: Int = 13
  order(66)                                       //> res9: Int = 10
  order(900)                                      //> res10: Int = 10
  order(50)                                       //> res11: Int = 12
  order(60)                                       //> res12: Int = 10
  order(244)                                      //> res13: Int = 14
  order(-4)                                       //> res14: Int = 0
  order(63)                                       //> res15: Int = 9
  
  
  /* Problem 3: Number Species
   * The problem with this implementation is that the result type is set to AnyVal
   * instead of a specified Integer value. This can be a a hazard when doing type
   * references and / or type checking. The way to fix this problem is to set the
   * return type equal to the function declaration block and add and else statment
   * to the outer conditional.
   */
  def species(n: Int): Int = {
   if (0 < n) if (n % 2 == 0) 1 else 2
   else 2
  }                                               //> species: (n: Int)Int
  
   species(50)                                    //> res16: Int = 1
   species(51)                                    //> res17: Int = 2
   
   
   
   // Problem 4: Elbonian Tax Calculator
	def taxCalc(x: Double): Double =
	{
		if (x < 0) throw new Exception("Income must be positive in order to calculate tax.")
		x match {
			case x if x < 20000 => 0
			case x if x < 30000 => x * .05
			case x if x < 40000 => x* .11
			case x if x < 60000 => x * .23
			case x if x < 100000 => x * .32
			case x if x >= 100000 => x *.5
		}
	}                                         //> taxCalc: (x: Double)Double


	taxCalc(5500)                             //> res18: Double = 0.0
	taxCalc(150000)                           //> res19: Double = 75000.0
	taxCalc(23905)                            //> res20: Double = 1195.25
	taxCalc(55001)                            //> res21: Double = 12650.230000000001
	taxCalc(89000)                            //> res22: Double = 28480.0
	taxCalc(34678)                            //> res23: Double = 3814.58
	taxCalc(12300)                            //> res24: Double = 0.0
	taxCalc(125000)                           //> res25: Double = 62500.0
	try {
		taxCalc(-660000)
	}
	catch {case e: Exception => println("Amount must be positive.")}
                                                  //> Amount must be positive.
                                                  //| res26: AnyVal = ()
	
	
	// Problem 5 5: drawRectangle
	def drawRectangle(n: Int, m: Int) {
		for (i <- 0 until n) {
			for (m <- 0 until m) {
				print("*")
			}
			print("\n")
		}
	}                                         //> drawRectangle: (n: Int, m: Int)Unit
	
	
	drawRectangle(3, 4)                       //> ****
                                                  //| ****
                                                  //| ****
  
  drawRectangle(5, 2)                             //> **
                                                  //| **
                                                  //| **
                                                  //| **
                                                  //| **
                                                  
  drawRectangle(1, 8)                             //> ********
  
  drawRectangle(9, 5)                             //> *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  
  
  /*
   * Problem 6: printSums
   */
  def printSums(n: Int, m: Int) {
  		for (i <- 0 until n; j <- 0 until m) println("" + i + " + " + j + " = " + (i + j))
  }                                               //> printSums: (n: Int, m: Int)Unit
  
  printSums(3, 4)                                 //> 0 + 0 = 0
                                                  //| 0 + 1 = 1
                                                  //| 0 + 2 = 2
                                                  //| 0 + 3 = 3
                                                  //| 1 + 0 = 1
                                                  //| 1 + 1 = 2
                                                  //| 1 + 2 = 3
                                                  //| 1 + 3 = 4
                                                  //| 2 + 0 = 2
                                                  //| 2 + 1 = 3
                                                  //| 2 + 2 = 4
                                                  //| 2 + 3 = 5

  printSums(9, 2)                                 //> 0 + 0 = 0
                                                  //| 0 + 1 = 1
                                                  //| 1 + 0 = 1
                                                  //| 1 + 1 = 2
                                                  //| 2 + 0 = 2
                                                  //| 2 + 1 = 3
                                                  //| 3 + 0 = 3
                                                  //| 3 + 1 = 4
                                                  //| 4 + 0 = 4
                                                  //| 4 + 1 = 5
                                                  //| 5 + 0 = 5
                                                  //| 5 + 1 = 6
                                                  //| 6 + 0 = 6
                                                  //| 6 + 1 = 7
                                                  //| 7 + 0 = 7
                                                  //| 7 + 1 = 8
                                                  //| 8 + 0 = 8
                                                  //| 8 + 1 = 9
                                                  
  printSums(1, 5)                                 //> 0 + 0 = 0
                                                  //| 0 + 1 = 1
                                                  //| 0 + 2 = 2
                                                  //| 0 + 3 = 3
                                                  //| 0 + 4 = 4
  
  
  
  
  /*
   * Problem 8: Realm Exception Handling
   */
  def realm1(n: Int): Int = if (n % 2 == 1) 1 else throw new Exception("Number not in Realm 1.") // = 1 if n belongs to realm 1, throws an exception otherwise
                                                  //> realm1: (n: Int)Int

  def realm2(n: Int): Int = if (n % 2 == 0 && n % 3 == 1) 2 else throw new Exception("Number not in Realm 2.") // = 2 if n belongs to realm 2, throws an exception otherwise
                                                  //> realm2: (n: Int)Int

  def realm3(n: Int): Int = if (n % 6 == 0 && n % 7 == 0) 3 else throw new Exception("Number not in Realm 3.") // = 3 if n belongs to realm 3, throws an exception otherwise
                                                  //> realm3: (n: Int)Int

  def realm(n: Int): Int  = {
  		var realm = 0
  		try {realm = realm1(n)} catch {case e: Exception => println("Not in Realm 1.")}
  		try {realm = realm2(n)} catch {case e: Exception => println("Not in Realm 2.")}
  		try {realm = realm3(n)} catch {case e: Exception => println("Not in Realm 3.")}
  		realm
  } // = the realm of n                           //> realm: (n: Int)Int
  
  realm(42)                                       //> Not in Realm 1.
                                                  //| Not in Realm 2.
                                                  //| res27: Int = 3
  
  realm(55)                                       //> Not in Realm 2.
                                                  //| Not in Realm 3.
                                                  //| res28: Int = 1
  
  
  realm(84)                                       //> Not in Realm 1.
                                                  //| Not in Realm 2.
                                                  //| res29: Int = 3
  
  
  realm(88)                                       //> Not in Realm 1.
                                                  //| Not in Realm 3.
                                                  //| res30: Int = 2
  
  
  
  realm(66)                                       //> Not in Realm 1.
                                                  //| Not in Realm 2.
                                                  //| Not in Realm 3.
                                                  //| res31: Int = 0
}