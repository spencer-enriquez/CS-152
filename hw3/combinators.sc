object combinators {
	def id[T](x: T) = x                       //> id: [T](x: T)T
  /*
   * Problem 1: Compose f(g(x)))
   * Create two more generic types for Dg and Rf
   */
  
  def compose [T, S, R](f: S => R, g: T => S): T=>R = {
   def r(x: T): R = f(g(x))
   r _
  }                                               //> compose: [T, S, R](f: S => R, g: T => S)T => R
  
  /*
   * Problem 2: Self-Iter
   * f composed with itself n times, where selfIter returns a function rather than a value.
   * Since compose returns a function T=>R, can only use inside of selfIter if this function also
   * returns a function. This contradicts the hint that selfIter(inc, 0) = id(x) = 0, since the selfIter
   * function no longer returns the generic type T.
   */
   def selfIter[T](f: T=>T, n: Int): T=>T = {
   	if (n == 0) id _ else compose(f, selfIter(f,n-1))
   	//def r (x: T, n: Int): T = if (n == 0) id(x) else r(f(x), n - 1)
   	//r (0, n)
   }                                              //> selfIter: [T](f: T => T, n: Int)T => T
   
   def inc(x: Double) = x + 1                     //> inc: (x: Double)Double
   def double(x: Double) = 2 * x                  //> double: (x: Double)Double
   val incBy5 = selfIter(inc, 5)                  //> incBy5  : Double => Double = combinators$$$Lambda$10/659748578@2133c8f8
   val doubleThrice = selfIter(double, 3)         //> doubleThrice  : Double => Double = combinators$$$Lambda$10/659748578@3ac3fd8
                                                  //| b

	 
	 incBy5(10)                               //> res0: Double = 15.0
	 doubleThrice(5)                          //> res1: Double = 40.0
	 doubleThrice(0)                          //> res2: Double = 0.0
	 selfIter(inc, 0)                         //> res3: Double => Double = combinators$$$Lambda$9/1802598046@6a5fc7f7
   
   
   /*
    * Problem 3: Count Pass
    */
    def countPass [T] (array: Array[T], f: T => Boolean): Int = {
    		val tSeq = array.filter(f)
    		tSeq.size
    }                                             //> countPass: [T](array: Array[T], f: T => Boolean)Int
    
    def divBy2(seq: Array[Int]): Int = countPass(seq, (x: Int)=>x%2 == 0)
                                                  //> divBy2: (seq: Array[Int])Int
    def containsTwo(seq: Array[String]): Int = countPass(seq, (x: String)=> x.contains("one"))
                                                  //> containsTwo: (seq: Array[String])Int
    val numSeq = Array(1,2,3,4,5,6,7,8,9)         //> numSeq  : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val numSeq2 = Array(1,3,5,7,9)                //> numSeq2  : Array[Int] = Array(1, 3, 5, 7, 9)
    val stringSeq = Array("one", "two", "three", "four")
                                                  //> stringSeq  : Array[String] = Array(one, two, three, four)
    val stringSeq2 = Array("one", "two", "one", "four", "one")
                                                  //> stringSeq2  : Array[String] = Array(one, two, one, four, one)
    
    divBy2(numSeq)                                //> res4: Int = 4
    divBy2(numSeq2)                               //> res5: Int = 0
    containsTwo(stringSeq)                        //> res6: Int = 1
    containsTwo(stringSeq2)                       //> res7: Int = 3
   
   
   /*
    * Problem 4: Combiner
    */
    def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
    		def r(x: Int): Int = if (x <= baseVal) baseVal else combiner(x, r(x -1))
    		r _
    	}                                         //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int
    	
    	val fact = recur(1, (x: Int, y: Int) => x * y)
                                                  //> fact  : Int => Int = combinators$$$Lambda$21/1622006612@614c5515
    fact (0)                                      //> res8: Int = 1
    fact (1)                                      //> res9: Int = 1
    fact (2)                                      //> res10: Int = 2
    fact (3)                                      //> res11: Int = 6
    	fact (4)                                  //> res12: Int = 24
    	fact (5)                                  //> res13: Int = 120
    	fact(6)                                   //> res14: Int = 720
    	
    	
    	/*
    	 * Problem 5: deOptionize
    	 */
    	 def deOptionize [T, S](f: S=>Option[T]): S=>T = {
    	 		def r(x: S): T = if(f(x) == None) throw new Exception("Error: Undefined Value") else f(x).get
    			r _
    	 }                                        //> deOptionize: [T, S](f: S => Option[T])S => T
    	 
    	 def parseDigits(digits: String): Option[Int] = if (digits.matches("[0-9]*")) Some(digits.toInt) else None
                                                  //> parseDigits: (digits: String)Option[Int]
    	 val parseDigits2 = deOptionize(parseDigits)
                                                  //> parseDigits2  : String => Int = combinators$$$Lambda$23/760563749@68837a77
    	 
    	 parseDigits2("123435")                   //> res15: Int = 123435
    	 parseDigits2("4423")                     //> res16: Int = 4423
    	 parseDigits2("1029274386")               //> res17: Int = 1029274386
    	 //parseDigits2("091.0")                  > java.lang.Exception: Error: Undefined Value
    	
    	 
    	 /*
    	  * Problem 6: Square Combinator
    	  */
   		def iterGen [T](init: T, n: Int, f: T=>T): T = {
   			if (n == 0) id(init)
   			else iterGen(f(init), n - 1, f)
   		}                                 //> iterGen: [T](init: T, n: Int, f: T => T)T
   		
   		def iterSquare(init: Double, n: Int): Double = iterGen(init, n, (x: Double) => x * x)
                                                  //> iterSquare: (init: Double, n: Int)Double
   		iterSquare(4,5)                   //> res18: Double = 1.8446744073709552E19
   		iterSquare(2,3)                   //> res19: Double = 256.0
   		iterSquare(0,5)                   //> res20: Double = 0.0
   		iterSquare(5, 0)                  //> res21: Double = 5.0
   		
   		
   		
   		/*
   		 * Problem 7: Unit Test
   		 */
   		 def unitTest[T, S](f: T=>S, list : Seq[(T,T)]): Int = {
   		 	def r(x: Int, shortList: Seq[(T,T)]): Int = {
   		 		if (shortList == Nil) x
   		 		else if (f(shortList.head._1) == shortList.head._2) r(x + 1, shortList.tail)
   		 		else r(x, shortList.tail)
   		 	}
   		 	r(0, list)
   		 }                                //> unitTest: [T, S](f: T => S, list: Seq[(T, T)])Int
   		 
   		 def cube(n: Int) = n * n * n     //> cube: (n: Int)Int
   		 unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124)))
                                                  //> res22: Int = 3
   		 
   		 unitTest(cube, Array((0, 5), (3, 27), (5, 5), (7, 343), (5, 125)))
                                                  //> res23: Int = 3
   		 
   		 unitTest(cube, Array((1, 1), (1, 1), (1, 1), (1, 1), (1, 1)))
                                                  //> res24: Int = 5
   		 unitTest(cube, Array((2,8)))     //> res25: Int = 1
   		 unitTest(cube, Array((5,5), (2,2)))
                                                  //> res26: Int = 0
    	 
  }