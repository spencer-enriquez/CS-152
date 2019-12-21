object fibonacci_combinations {
  /*
   * Problem 9: Recursive Fibonacci Sequence
   */
   def fib(n: Int): Int = {
   	if(n == 0 || n == 1) n else fib(n - 1) + fib(n - 2)
   }                                              //> fib: (n: Int)Int
   
   fib(0)                                         //> res0: Int = 0
   fib(1)                                         //> res1: Int = 1
   fib(2)                                         //> res2: Int = 1
   fib(3)                                         //> res3: Int = 2
   fib(4)                                         //> res4: Int = 3
   fib(5)                                         //> res5: Int = 5
   fib(6)                                         //> res6: Int = 8
   fib(7)                                         //> res7: Int = 13
   fib(15)                                        //> res8: Int = 610
   fib(25)                                        //> res9: Int = 75025
   
   
   def fibTail(n: Int): Int = {
   	def helper(count: Int, previous: Int, current: Int): Int = if (count == 0) previous else helper(count - 1, current, current + previous)
   	helper(n, 0, 1)
   }                                              //> fibTail: (n: Int)Int
   
   fibTail(0)                                     //> res10: Int = 0
   fibTail(1)                                     //> res11: Int = 1
   fibTail(2)                                     //> res12: Int = 1
   fibTail(3)                                     //> res13: Int = 2
   fibTail(4)                                     //> res14: Int = 3
   fibTail(5)                                     //> res15: Int = 5
   fibTail(6)                                     //> res16: Int = 8
   fibTail(7)                                     //> res17: Int = 13
   fibTail(15)                                    //> res18: Int = 610
   fibTail(25)                                    //> res19: Int = 75025
   
   
  /*
   * Problem 10: Recursive Combinatorics (Traditional)
   */
   def choose(n: Int, m: Int): Int = {
   	 if (m == 1) n else if (n == m) 1 else if (n == 0 || m == 0) 0 else choose(n - 1, m - 1) + choose(n - 1, m)
   }                                              //> choose: (n: Int, m: Int)Int
   
   choose(5, 2)                                   //> res20: Int = 10
   choose(10, 4)                                  //> res21: Int = 210
   choose(6, 0)                                   //> res22: Int = 0
   choose(6, 5)                                   //> res23: Int = 6
   choose(9, 1)                                   //> res24: Int = 9
   choose(20 , 10)                                //> res25: Int = 184756
   //choose()
}