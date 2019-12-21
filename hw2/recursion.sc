object recursion {
	def inc(n: Int) = n + 1                   //> inc: (n: Int)Int
	def dec(n: Int) = n - 1                   //> dec: (n: Int)Int
	def isZero(n: Int) = n == 0               //> isZero: (n: Int)Boolean
	
	
	/*
	 * Problem 1: Recursive Addition
	 */
	 def add(n: Int, m: Int): Int = if (isZero(m)) n else inc(add(n, dec(m)))
                                                  //> add: (n: Int, m: Int)Int
                                                  
   add(5,6)                                       //> res0: Int = 11
   add(78, 53)                                    //> res1: Int = 131
   add(75, 0)                                     //> res2: Int = 75
   add(0,23)                                      //> res3: Int = 23
   
   
   /*
    * Problem 2: Recursive Multiplication
    */
    def mul(n: Int, m: Int): Int = if (isZero(m)) 0 else add(n, mul(n, dec(m)))
                                                  //> mul: (n: Int, m: Int)Int
    
    
    mul(4,3)                                      //> res4: Int = 12
    mul(25, 5)                                    //> res5: Int = 125
    mul(1, 17)                                    //> res6: Int = 17
    mul(0, 9)                                     //> res7: Int = 0
    
    
   /*
    * Problem 3: Recursive
    */
    def exp2(m: Int): Int = if (isZero(m)) 1 else mul(2, exp2(dec(m)))
                                                  //> exp2: (m: Int)Int
    
    exp2(2)                                       //> res8: Int = 4
    exp2(4)                                       //> res9: Int = 16
    exp2(8)                                       //> res10: Int = 256
    exp2(0)                                       //> res11: Int = 1
    
    
    /*
     * Problem 4: Recursive Hyper-Exponentiation
     */
     def hyperExp(n: Int): Int = if (isZero(n)) 0 else exp2(hyperExp(dec(n)))
                                                  //> hyperExp: (n: Int)Int
     
     hyperExp(0)                                  //> res12: Int = 0
     hyperExp(1)                                  //> res13: Int = 1
     hyperExp(2)                                  //> res14: Int = 2
     hyperExp(3)                                  //> res15: Int = 4
     hyperExp(4)                                  //> res16: Int = 16
   	 hyperExp(5)                              //> java.lang.StackOverflowError
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add$1(recursion.scala:10)
                                                  //| 	at recursion$.add
                                                  //| Output exceeds cutoff limit.
   
   
}