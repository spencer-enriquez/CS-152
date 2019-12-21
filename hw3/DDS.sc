object DDS {
	// Control Loop
  def controlLoop[S] (state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S = {
  		if (halt(state, cycle)) state
  		else controlLoop(update(state, cycle), cycle + 1, halt, update)
  }                                               //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
                                                  //| , Int) => S)S
  /*
  // Example 1
  def growth(pop : Double, time: Int) = pop + .2 * pop -.3 * pop
  def timesUp(pop: Double, time: Int) = time == 100 || pop == 0
  controlLoop(10.0, 5, timesUp, growth)
  
  // Example 2: Solve
  def solve(f: Double=>Double): Double = {
  		val delta = 1e-10
  		def goodEnough(guess: Double, time: Int) = math.abs(f(guess)) <= delta
  		def improve(guess: Double, time: Int): Double = guess  - f(guess) / df(guess)
  		def df(x: Double) = (f(x + delta) - f(x)) / delta
  		controlLoop(1.0, 0, goodEnough, improve)
  }
  
  solve((x: Double) => x*2 - x - 20)
  
  // Example 3: Sqrt X
  def sqrt(x: Double) = solve((y: Double) => y * y - x)
  sqrt(49)
  // nth root: y^n - x
  */
  
  
  /*
   * Problem 8.2: Amoeba Growth
   */
  def amoebaGrowth(pop : Int, time: Int) = pop + pop
                                                  //> amoebaGrowth: (pop: Int, time: Int)Int
	def amoebaTime(pop: Int, time: Int) = pop > 100000
                                                  //> amoebaTime: (pop: Int, time: Int)Boolean
	controlLoop(100, 0, amoebaTime, amoebaGrowth)
                                                  //> res0: Int = 102400
	controlLoop(50, 0, amoebaTime, amoebaGrowth)
                                                  //> res1: Int = 102400
	controlLoop(1, 0, amoebaTime, amoebaGrowth)
                                                  //> res2: Int = 131072
	controlLoop(17, 0, amoebaTime, amoebaGrowth)
                                                  //> res3: Int = 139264
	controlLoop(7, 0, amoebaTime, amoebaGrowth)
                                                  //> res4: Int = 114688
	
	
	/*
	 * Problem 8.3: Finding the Roots of Functions
	 */
	 def solve(f: Double=>Double): Double = {
  		val delta = 1e-10
  		def goodEnough(guess: Double, time: Int) = math.abs(f(guess)) <= delta
  		def improve(guess: Double, time: Int): Double = guess  - f(guess) / df(guess)
  		def df(x: Double) = (f(x + delta) - f(x)) / delta
  		controlLoop(1.0, 0, goodEnough, improve)
  }                                               //> solve: (f: Double => Double)Double
  
  
  solve((x: Double) => x*2 - x - 20)              //> res5: Double = 19.999999999999893
  
  
  
  /*
   * Problem 8.4: Square Roots
   */
  	def sqrt(x: Double) = solve((y: Double) => y * y - x)
                                                  //> sqrt: (x: Double)Double
  	
  	sqrt(49)                                  //> res6: Double = 6.999999999999653
  	sqrt(55)                                  //> res7: Double = 7.416198487096809
  	sqrt(82)                                  //> res8: Double = 9.055385138137417
  	sqrt(64)                                  //> res9: Double = 7.999999999999968
  	sqrt(3)                                   //> res10: Double = 1.7320508075688794
  	sqrt(1)                                   //> res11: Double = 1.0
  	
  	/*
  	 * Problem 8.5: Cube Roots
  	 */
  	 def cubeRoot(x: Double) = solve((y: Double) => y * y * y - x)
                                                  //> cubeRoot: (x: Double)Double
   cubeRoot(49)                                   //> res12: Double = 3.6593057100227893
  	 cubeRoot(55)                             //> res13: Double = 3.8029524607618406
  	 cubeRoot(82)                             //> res14: Double = 4.344481485768695
   cubeRoot(64)                                   //> res15: Double = 4.0
  	 cubeRoot(3)                              //> res16: Double = 1.4422495703072642
  	 cubeRoot(1)                              //> res17: Double = 1.0
  	 
  	 /*
  	  * Problem 8.6: Nth Roots
  	  */
  	  def nthRoot(x: Double, n: Int) = solve((y: Double) => math.pow(y,n) - x)
                                                  //> nthRoot: (x: Double, n: Int)Double
    nthRoot(49, 2)                                //> res18: Double = 6.999999999999653
  	  nthRoot(55, 4)                          //> res19: Double = 2.7232698153313626
  	  nthRoot(82, 6)                          //> res20: Double = 2.084341979083227
    nthRoot(64, 8)                                //> res21: Double = 1.6817928305074288
  	  nthRoot(3, 3)                           //> res22: Double = 1.4422495703074203
  	  nthRoot(1, 20)                          //> res23: Double = 1.0
  	  
  	  
  	  /*
  	   * Problem 8.7: Compound Interest
  	   */
  	   def ciMonthly(amount: Double, time: Int) = amount * (1 + .083333333333)
                                                  //> ciMonthly: (amount: Double, time: Int)Double
  	   def ciDaily(amount: Double, time: Int) = amount * (1 + .002739726)
                                                  //> ciDaily: (amount: Double, time: Int)Double
  	   def ciHourly(amount: Double, time: Int) = amount * (1 + .0001141552511)
                                                  //> ciHourly: (amount: Double, time: Int)Double
  	   def ciPerSecond(amount: Double, time: Int) = amount * (1 + .00000003170979198)
                                                  //> ciPerSecond: (amount: Double, time: Int)Double
  	   
  	   def haltMonthly(amount: Double, time: Int) = time == 12
                                                  //> haltMonthly: (amount: Double, time: Int)Boolean
  	   def haltDaily(amount: Double, time: Int) = time == 365
                                                  //> haltDaily: (amount: Double, time: Int)Boolean
  	   def haltHourly(amount: Double, time: Int) = time == 8760
                                                  //> haltHourly: (amount: Double, time: Int)Boolean
  	   def haltPerSecond(amount: Double, time: Int) = time == 31536000
                                                  //> haltPerSecond: (amount: Double, time: Int)Boolean
  	   
  	   controlLoop(1.0, 5, haltMonthly, ciMonthly)
                                                  //> res24: Double = 1.7511959485791182
  	   controlLoop(1.0, 0, haltDaily, ciDaily)//> res25: Double = 2.7145674549503114
  	   controlLoop(1.0, 0, haltHourly, ciHourly)
                                                  //> res26: Double = 2.7181266906293495
  	   controlLoop(1.0, 0, haltPerSecond, ciPerSecond)
                                                  //> res27: Double = 2.718281778468765
}