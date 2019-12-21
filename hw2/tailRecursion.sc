/*
 * Problem 5: Tail Recursive Arithmetic Functions
 * Since the tail recursive form is similar to iterative solutions in terms of space complexity O(n), the stack overflow issue is reduced to reduced amount of saved
 * recursive calls. With tail recursion, it allows the program to hold one value within the stack trace to update rather than keeping all possible recursive values.
 * In terms of computation time, each function runs in polynomial time O(n).
 */
object tailRecursion {
	def inc(n: Int) = n + 1                   //> inc: (n: Int)Int
	def dec(n: Int) = n - 1                   //> dec: (n: Int)Int
	def isZero(n: Int) = n == 0               //> isZero: (n: Int)Boolean
	
  def add(n: Int, m: Int): Int = {
  		//if (isZero(m)) n else add(inc(n) , dec(m))
  		def helper(count: Int, result: Int): Int = if (isZero(count)) result else helper(dec(count), inc(result))
  		helper(n,m)
  	}                                         //> add: (n: Int, m: Int)Int
  
  add(5,6)                                        //> res0: Int = 11
  add(78, 53)                                     //> res1: Int = 131
  add(75, 0)                                      //> res2: Int = 75
  add(0,23)                                       //> res3: Int = 23
  
  
  def mul(n: Int, m: Int): Int = {
  		//if (isZero(m)) 1 else mul(dec(n), add(n, m))
  		def helper(count: Int, result: Int): Int = if (isZero(count)) result else helper(dec(count), add(result, m))
  		helper(n,0)
  	}                                         //> mul: (n: Int, m: Int)Int
  
  mul(4,3)                                        //> res4: Int = 12
  mul(25, 5)                                      //> res5: Int = 125
  mul(1, 17)                                      //> res6: Int = 17
  mul(0, 9)                                       //> res7: Int = 0
  
  
  def exp2(m: Int): Int = {
  		//if (isZero(m)) 1 else mul(2, exp2(dec(m)))
  		def helper(count: Int, result: Int): Int = if (isZero(count)) result else helper(dec(count), mul(result, 2))
  		helper(m,1)
  	}                                         //> exp2: (m: Int)Int
  	
  	
  	exp2(2)                                   //> res8: Int = 4
  exp2(4)                                         //> res9: Int = 16
  exp2(8)                                         //> res10: Int = 256
  exp2(0)                                         //> res11: Int = 1
  
  
  def hyperExp(n: Int): Int = {
  		//if (isZero(n)) 1 else exp2(hyperExp(dec(n)))
  		def helper(count: Int, result: Int): Int = if (isZero(count)) result else helper(dec(count), exp2(result))
  		helper(n,0)
  	}                                         //> hyperExp: (n: Int)Int
  	
  	
  	hyperExp(0)                               //> res12: Int = 0
  hyperExp(1)                                     //> res13: Int = 1
  hyperExp(2)                                     //> res14: Int = 2
  hyperExp(3)                                     //> res15: Int = 4
  hyperExp(4)                                     //> res16: Int = 16
  hyperExp(5)                                     //> res17: Int = 65536
}