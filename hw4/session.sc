object session {
  /*
   * Problem 1: Sum of Cubes of Odd Numbers
   */
   def oddCubeSum1(list: List[Int]): Int = {
   	var sum = 0;
   	for (cell <- list) sum = if (cell % 2 == 1) sum + cell * cell * cell else sum
   	sum
   }                                              //> oddCubeSum1: (list: List[Int])Int
   
   oddCubeSum1(List(1,2,3,4,5,6,7,8,9))           //> res0: Int = 1225
   oddCubeSum1(List(2,4,6,8,10))                  //> res1: Int = 0
   
   // Classic Recursion
   def oddCubeSum2(list: List[Int]): Int = {
   	if (list == Nil) 0
   	else if (list.head % 2 == 1) list.head * list.head * list.head + oddCubeSum2(list.tail)
   	else oddCubeSum2(list.tail)
   }                                              //> oddCubeSum2: (list: List[Int])Int
   
   oddCubeSum2(List(1,2,3,4,5,6,7,8,9))           //> res2: Int = 1225
   oddCubeSum2(List(2,4,6,8,10))                  //> res3: Int = 0
   
   // Tail Recursion
   def oddCubeSum3(list: List[Int]): Int = {
   	def helper(result: Int,shortList: List[Int]): Int = {
   		if (shortList == Nil) result
   		else if (shortList.head % 2 == 1) helper(result + shortList.head * shortList.head * shortList.head, shortList.tail)
   		else helper(result, shortList.tail)
   	}
   	helper(0, list)
   }                                              //> oddCubeSum3: (list: List[Int])Int
   
   oddCubeSum3(List(1,2,3,4,5,6,7,8,9))           //> res4: Int = 1225
   oddCubeSum3(List(2,4,6,8,10))                  //> res5: Int = 0
   
   // Pipeline
   def cube(x: Int): Int = x*x*x                  //> cube: (x: Int)Int
   def oddCubeSum4(list: List[Int]): Int = list.filter(_ % 2 == 1).map(cube).reduce(_ + _)
                                                  //> oddCubeSum4: (list: List[Int])Int
   
   oddCubeSum4(List(1,2,3,4,5,6,7,8,9))           //> res6: Int = 1225
   oddCubeSum4(List(2,4,6,9,10))                  //> res7: Int = 729
   
  /*
   * Problem 2: Sum of Sums
   */
   def sumOfSums1(list: List[List[Int]]): Int = {
   	var result = 0
   	for (cell <- list)
   		for(value <- cell)
   			result = result + value
   	result
   }                                              //> sumOfSums1: (list: List[List[Int]])Int
   
   sumOfSums1(List(List(1, 2, 3), List(4, 5, 6))) //> res8: Int = 21
   sumOfSums1(List(List(1, 2, 3), List(4, 5, 6), List(15,17,28,0)))
                                                  //> res9: Int = 81
   
   // Classic Recursion
   def sumOfSums2(list: List[List[Int]]): Int = {
   	if (list == Nil) 0
   	else list.head.reduce(_ + _) + sumOfSums2(list.tail)
   }                                              //> sumOfSums2: (list: List[List[Int]])Int
   
   sumOfSums2(List(List(1, 2, 3), List(4, 5, 6))) //> res10: Int = 21
   sumOfSums2(List(List(1, 2, 3), List(4, 5, 6), List(15,17,28,0)))
                                                  //> res11: Int = 81
   
   // Tail Recursion
   def sumOfSums3(list: List[List[Int]]): Int = {
   	def helper(result: Int, short: List[List[Int]]): Int = {
   		if (short == Nil) result
   		else helper(result + short.head.reduce(_ + _), short.tail)
   	}
   	helper(0, list)
   }                                              //> sumOfSums3: (list: List[List[Int]])Int
   
   sumOfSums3(List(List(1, 2, 3), List(4, 5, 6))) //> res12: Int = 21
   sumOfSums3(List(List(1, 2, 3), List(4, 5, 6), List(15,17,28,0)))
                                                  //> res13: Int = 81
   
   // Pipeline
   def sumOfSums4(list: List[List[Int]]): Int = list.map(_.reduce(_ + _)).reduce(_ + _)
                                                  //> sumOfSums4: (list: List[List[Int]])Int
   
   sumOfSums4(List(List(1, 2, 3), List(4, 5, 6))) //> res14: Int = 21
   sumOfSums4(List(List(1, 2, 3), List(4, 5, 6), List(15,17,28,0)))
                                                  //> res15: Int = 81
   
   /*
    * Problem 3: Depth Counter
    */
   def depth(v: Any): Int = {
   	def helper(count: Int, w: Any, end: Any): Int = {
   		w match {
      		case Nil => count + 1
      		case h::t => helper(count + 1, w.asInstanceOf[List[Any]].head, w.asInstanceOf[List[Any]].tail)
      		case _ => count
			}
		}
		helper(0, v, Nil)
   }                                              //> depth: (v: Any)Int
   
   depth(List(List(List(List(3)))))               //> res16: Int = 4
   depth(List(Nil))                               //> res17: Int = 2
   depth(List(1,2,3,4,5,6))                       //> res18: Int = 1
   depth(5)                                       //> res19: Int = 0
   
   
   /*
    * Problem 4: Average Doubles
    */
    def average(list: List[Double]): Double = list.filter(_ > 0).reduce(_ + _) / list.filter(_ > 0).size
                                                  //> average: (list: List[Double])Double
    
    average(List(34.6, 435.9, 12.89, 12, 0.5))    //> res20: Double = 99.178
    average(List(100.65, 0.45, 92.67, 88.98, 1.55))
                                                  //> res21: Double = 56.86
    average(List(402145.89, 0, 0, 0, 0))          //> res22: Double = 402145.89
    average(List(402145.89, 0, 0, 0, -5, 0))      //> res23: Double = 402145.89
    
   /*
    * Problem 5: Largest Element
    * Relational operators compile towards the compare function
    */
    class Num(val value: Int) extends Ordered[Num] {
  		 def compare(other: Num): Int = this.value - other.value
   	 override def toString = "num(" + value + ")"
		}

		val n1 = new Num(3)               //> n1  : session.Num = num(3)
		val n2 = new Num(5)               //> n2  : session.Num = num(5)

		// n1 < n2 compiles into n1.compare(n2):
		n1 < n2                           //> res24: Boolean = true

		n2 < n1                           //> res25: Boolean = false

		val nums1 = List(new Num(5), new Num(3), new Num(-99), new Num(6), new Num(0))
                                                  //> nums1  : List[session.Num] = List(num(5), num(3), num(-99), num(6), num(0))
                                                  //| 
		val nums2 = List(new Num(15), new Num(4), new Num(-1), new Num(16), new Num(20))
                                                  //> nums2  : List[session.Num] = List(num(15), num(4), num(-1), num(16), num(20
                                                  //| ))


		def findMax[T](vals: List[Ordered[T]]) = {
			var max = vals(0)
			for (elem <- vals)
				if (max.asInstanceOf[Ordered[T]] < elem.asInstanceOf[T]) max = elem
			max.toString
		}                                 //> findMax: [T](vals: List[Ordered[T]])String
		
		findMax(nums1)                    //> res26: String = num(6)
		findMax(nums2)                    //> res27: String = num(20)

   
   /*
    * Problem 6:
    */
    def predicateCount1[T](list: List[T], f: T=>Boolean): Int = {
    		var count = 0
    		for (cell <- list)
    			if (f(cell)) count = count + 1
    		count
    }                                             //> predicateCount1: [T](list: List[T], f: T => Boolean)Int
    
    predicateCount1(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res28: Int = 5
    predicateCount1(List(5.89,10.23 ,-12.78, 1.123, -3.95, 143.4574, -8.9, 0, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res29: Int = 6
    predicateCount1(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res30: Int = 3
    
    // Classical Recursion
    def predicateCount2[T](list: List[T], f: T=>Boolean): Int = {
    		if (list == Nil) 0
    		else if (f(list.head)) 1 + predicateCount2(list.tail, f)
    		else predicateCount2(list.tail, f)
    }                                             //> predicateCount2: [T](list: List[T], f: T => Boolean)Int
    
    predicateCount2(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res31: Int = 5
    predicateCount2(List(5.89,10.23 ,-12.78, 1.123, -3.95, 143.4574, -8.9, 0, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res32: Int = 6
    predicateCount2(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res33: Int = 3
    
    // Tail Recursion
    def predicateCount3[T](list: List[T], f: T=>Boolean): Int = {
    		def helper(count: Int, short: List[T]): Int = {
    			if (short == Nil) count
    			else if (f(short.head)) helper(count + 1, short.tail)
    			else helper(count, short.tail)
    		}
    		helper (0, list)
    }                                             //> predicateCount3: [T](list: List[T], f: T => Boolean)Int
    
    predicateCount3(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res34: Int = 5
    predicateCount3(List(5.89,10.23 ,-12.78, 1.123, -3.95, 143.4574, -8.9, 0, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res35: Int = 6
    predicateCount3(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res36: Int = 3
    
    // Pipeline
    def predicateCount4[T](list: List[T], f: T=>Boolean): Int =  list.filter(f).size
                                                  //> predicateCount4: [T](list: List[T], f: T => Boolean)Int
    
    predicateCount4(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res37: Int = 5
    predicateCount4(List(5.89,10.23 ,-12.78, 1.123, -3.95, 143.4574, -8.9, 0, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res38: Int = 6
    predicateCount4(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res39: Int = 3
    
   /*
   * Problem 7: If all elements pass the predicate, return true, false otherwise
   */
   def predicateAllCheck1[T](list: List[T], f: T=>Boolean): Boolean = {
    		var result = true
    		for (cell <- list)
    			if (f(cell)) result = false
    		result
    }                                             //> predicateAllCheck1: [T](list: List[T], f: T => Boolean)Boolean
   
   predicateAllCheck1(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res40: Boolean = false
    predicateAllCheck1(List(5.89,10.23 ,12.78, 1.123, 3.95, 143.4574, -8.9, 9.1, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res41: Boolean = false
    predicateAllCheck1(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size >= 0)
                                                  //> res42: Boolean = false
   
   
   // Classical Recursion
    def predicateAllCheck2[T](list: List[T], f: T=>Boolean): Boolean = {
    		if (list == Nil) true
    		else if (f(list.head)) predicateAllCheck2(list.tail, f)
    		else false
    }                                             //> predicateAllCheck2: [T](list: List[T], f: T => Boolean)Boolean
   
   predicateAllCheck2(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res43: Boolean = false
    predicateAllCheck2(List(5.89,10.23 ,12.78, 1.123, 3.95, 143.4574, -8.9, 9.1, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res44: Boolean = false
    predicateAllCheck2(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size >= 0)
                                                  //> res45: Boolean = true
   
   
   // Tail Recursion
   def predicateAllCheck3[T](list: List[T], f: T=>Boolean): Boolean = {
    		def helper(count: Int, short: List[T]): Boolean = {
    			if (short == Nil) true
    			else if (f(short.head)) helper(count - 1, short.tail)
    			else false
    		}
    		helper (0, list)
    }                                             //> predicateAllCheck3: [T](list: List[T], f: T => Boolean)Boolean
   
    predicateAllCheck3(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res46: Boolean = false
    predicateAllCheck3(List(5.89,10.23 ,12.78, 1.123, 3.95, 143.4574, -8.9, 9.1, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res47: Boolean = false
    predicateAllCheck3(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size >= 0)
                                                  //> res48: Boolean = true
   
   
   // Pipeline
    def predicateAllCheck4[T](list: List[T], f: T=>Boolean): Boolean =  list.filter(f).size == list.size
                                                  //> predicateAllCheck4: [T](list: List[T], f: T => Boolean)Boolean
    
    predicateAllCheck4(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res49: Boolean = false
    predicateAllCheck4(List(5.89,10.23 ,12.78, 1.123, 3.95, 143.4574, -8.9, 9.1, 0.2 ,5.25), (x: Double) => x > 0)
                                                  //> res50: Boolean = false
    predicateAllCheck4(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size >= 0)
                                                  //> res51: Boolean = true
   
    
   /*
   * Problem 8: If any element passes the predicate, return true. False otherwise
   */
   def predicateCheck1[T](list: List[T], f: T=>Boolean): Boolean = {
    		var result = false
    		for (cell <- list)
    			if (f(cell)) result = true
    		result
    }                                             //> predicateCheck1: [T](list: List[T], f: T => Boolean)Boolean
   
   predicateCheck1(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res52: Boolean = true
    predicateCheck1(List(-5.89,-10.23 ,-12.78, -1.123, -3.95, -143.4574, -8.9, -0.1, -0.2 ,-5.25), (x: Double) => x > 0)
                                                  //> res53: Boolean = false
    predicateCheck1(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res54: Boolean = true
   
   // Classical Recursion
    def predicateCheck2[T](list: List[T], f: T=>Boolean): Boolean = {
    		if (list == Nil) false
    		else if (f(list.head)) true
    		else predicateCheck2(list.tail, f)
    }                                             //> predicateCheck2: [T](list: List[T], f: T => Boolean)Boolean
   
   predicateCheck2(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res55: Boolean = true
    predicateCheck2(List(-5.89,-10.23 ,-12.78, -1.123, -3.95, -143.4574, -8.9, -0.1, -0.2 ,-5.25), (x: Double) => x > 0)
                                                  //> res56: Boolean = false
    predicateCheck2(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res57: Boolean = true
   
   
   // Tail Recursion
   def predicateCheck3[T](list: List[T], f: T=>Boolean): Boolean = {
    		def helper(count: Int, short: List[T]): Boolean = {
    			if (short == Nil) false
    			else if (f(short.head)) true
    			else helper(count, short.tail)
    		}
    		helper (0, list)
    }                                             //> predicateCheck3: [T](list: List[T], f: T => Boolean)Boolean
    
    predicateCheck3(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res58: Boolean = true
    predicateCheck3(List(-5.89,-10.23 ,-12.78, -1.123, -3.95, -143.4574, -8.9, -0.1, -0.2 ,-5.25), (x: Double) => x > 0)
                                                  //> res59: Boolean = false
    predicateCheck3(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res60: Boolean = true
   
   // Pipeline
    def predicateCheck4[T](list: List[T], f: T=>Boolean): Boolean =  list.filter(f).size > 0
                                                  //> predicateCheck4: [T](list: List[T], f: T => Boolean)Boolean
    
    predicateCheck4(List(5,10 ,-12, 1, -3, 143, -8, 0, 0 ,5), (x: Int) => x > 0)
                                                  //> res61: Boolean = true
    predicateCheck4(List(-5.89,-10.23 ,-12.78, -1.123, -3.95, -143.4574, -8.9, -0.1, -0.2 ,-5.25), (x: Double) => x > 0)
                                                  //> res62: Boolean = false
    predicateCheck4(List("askjgf", "sd", "F", "", "asdgfksdangfl", "sdlkjn", "sd"), (x: String) => x.size > 3)
                                                  //> res63: Boolean = true
   
   /*
    * Problem 9: Reverse List
    */
    def reverse[T](list: List[T]): List[T] = {
    		var result = List[T]()
    		for(i <- 0 until list.size) {
    			val temp = list(i) +: result
    			result = temp
    		}
    		result
    }                                             //> reverse: [T](list: List[T])List[T]
    
    reverse(List(1,2,3,4,5,6,7,8,9))              //> res64: List[Int] = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
    reverse(List("tooth", "cat", "dog", "fish", "fork"))
                                                  //> res65: List[String] = List(fork, fish, dog, cat, tooth)
    
    
    /*
     * Problem 10: isSorted (Integer) in Ascending Order
     */
     def isSorted(list: List[Int]): Boolean = {
     	var result = true
     	for(i <- 0 until list.size) {
     		var base = list(i)
     		for(j <- i until list.size) {
     			if (math.min(base, list(j)) != base) result = false
     		}
     	}
     	result
     }                                            //> isSorted: (list: List[Int])Boolean
     
     isSorted(List(2,2,3,3,4,4,5,6,7,8))          //> res66: Boolean = true
     isSorted(List(1,2,3,4,5,3,6,7,8,9))          //> res67: Boolean = false
     isSorted(List(9,8,7,6,5,4,3,2,1,0))          //> res68: Boolean = false
   	
   	
   	
   	/*
   	 * Problem 11: My Map
   	 */
   	 def myMap[S, T](f: S=>T, list: List[S]): List[T] = {
   	 	var result = List[T]()
    		for(i <- 0 until list.size) {
    			val temp = result :+ f(list(i))
    			result = temp
    		}
    		result
   	 }                                        //> myMap: [S, T](f: S => T, list: List[S])List[T]
   	 
   	 def numToSquare(x: Int): Int = x * x     //> numToSquare: (x: Int)Int
   	 def numToSquareRoot(x: Int): Double = math.sqrt(x)
                                                  //> numToSquareRoot: (x: Int)Double
   	 val list1 = List(1,2,3,4,5,6,7,8,9)      //> list1  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
   	 val list2 = List(11,12,13,14,15,16,17,18,19)
                                                  //> list2  : List[Int] = List(11, 12, 13, 14, 15, 16, 17, 18, 19)
   	 
   	 myMap(numToSquare, list1)                //> res69: List[Int] = List(1, 4, 9, 16, 25, 36, 49, 64, 81)
   	 myMap(numToSquare, list2)                //> res70: List[Int] = List(121, 144, 169, 196, 225, 256, 289, 324, 361)
   	 
   	 myMap(numToSquareRoot, list1)            //> res71: List[Double] = List(1.0, 1.4142135623730951, 1.7320508075688772, 2.
                                                  //| 0, 2.23606797749979, 2.449489742783178, 2.6457513110645907, 2.828427124746
                                                  //| 1903, 3.0)
   	 myMap(numToSquareRoot, list2)            //> res72: List[Double] = List(3.3166247903554, 3.4641016151377544, 3.60555127
                                                  //| 5463989, 3.7416573867739413, 3.872983346207417, 4.0, 4.123105625617661, 4.
                                                  //| 242640687119285, 4.358898943540674)
     
     
     /*
      * Problem 12: Take, Drop, Zip
      */
     def take[T](list: List[T], num: Int): List[T] = {
     	var result = List[T]()
     	for(i <- 0 until num)
     		result = result :+ list(i)
     	result
     }                                            //> take: [T](list: List[T], num: Int)List[T]
     
     def drop[T](list: List[T], num: Int): List[T] = {
     	var result = List[T]()
     	for(i <- num until list.size)
     		result = result :+ list(i)
     	result
     }                                            //> drop: [T](list: List[T], num: Int)List[T]
     
     def zip[T,S](list1: List[T], list2: List[T]): List[(T, T)] =
     {
     	var result = List[(T, T)]()
     	for (i <- 0 until math.min(list1.size, list2.size))
     		result = result :+ (list1(i), list2(i))
     	result
     }                                            //> zip: [T, S](list1: List[T], list2: List[T])List[(T, T)]
     
     val listA = List(1,2,3,4,5,6,7,8,9)          //> listA  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
     val listB = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
                                                  //> listB  : List[String] = List(a, b, c, d, e, f, g, h, i, j, k, l)
     
     take(listA, 5)                               //> res73: List[Int] = List(1, 2, 3, 4, 5)
     drop(listA, 5)                               //> res74: List[Int] = List(6, 7, 8, 9)
     
     take(listB, 3)                               //> res75: List[String] = List(a, b, c)
     drop(listB, 7)                               //> res76: List[String] = List(h, i, j, k, l)
     zip(listA, listB)                            //> res77: List[(Any, Any)] = List((1,a), (2,b), (3,c), (4,d), (5,e), (6,f), (
                                                  //| 7,g), (8,h), (9,i))
     
     /*
      * Problem 13: Streams
      */
      
      def makeOnes: Stream[Int] = 1 #:: makeOnes  //> makeOnes: => Stream[Int]
      def nonNegatives(x: Int): Stream[Int] = x #:: nonNegatives(x + 1)
                                                  //> nonNegatives: (x: Int)Stream[Int]
      def evens(x: Int): Stream[Int] = x #:: evens(x + 2)
                                                  //> evens: (x: Int)Stream[Int]
      def squares(x: Int): Stream[Int] = (x * x) #:: squares(x + 1)
                                                  //> squares: (x: Int)Stream[Int]
      
      val ones = makeOnes                         //> ones  : Stream[Int] = Stream(1, ?)
      ones.tail.tail.tail                         //> res78: scala.collection.immutable.Stream[Int] = Stream(1, ?)
      ones                                        //> res79: Stream[Int] = Stream(1, 1, 1, 1, ?)
      
      
      val notNegative = nonNegatives(0)           //> notNegative  : Stream[Int] = Stream(0, ?)
      notNegative.tail.tail.tail                  //> res80: scala.collection.immutable.Stream[Int] = Stream(3, ?)
      notNegative                                 //> res81: Stream[Int] = Stream(0, 1, 2, 3, ?)
      
      val even = evens(0)                         //> even  : Stream[Int] = Stream(0, ?)
      even.tail.tail.tail                         //> res82: scala.collection.immutable.Stream[Int] = Stream(6, ?)
      even                                        //> res83: Stream[Int] = Stream(0, 2, 4, 6, ?)
      
      val square = squares(0)                     //> square  : Stream[Int] = Stream(0, ?)
      square.tail.tail.tail                       //> res84: scala.collection.immutable.Stream[Int] = Stream(9, ?)
      square                                      //> res85: Stream[Int] = Stream(0, 1, 4, 9, ?)
      
      
      
      /*
       * Problem 14: Exam Scores
       */
       def avg(scores: List[Double]): Double = scores.reduce(_ + _) / scores.size
                                                  //> avg: (scores: List[Double])Double
       def avgAvg(scores: List[List[Double]]): List[Double] = scores.map(avg(_))
                                                  //> avgAvg: (scores: List[List[Double]])List[Double]
       def passing(scores: List[List[Double]]): List[Int] = {
       	val avg = avgAvg(scores)
       	var pos = List[Int]()
       	for(i <- 0 until avg.size)
       		if (avg(i) >= 70) pos = pos :+ i
       	pos
       }                                          //> passing: (scores: List[List[Double]])List[Int]
       def sumSums(scores: List[List[Double]]):Double = scores.map(_.reduce(_ + _)).reduce(_ + _)
                                                  //> sumSums: (scores: List[List[Double]])Double
       
       val cs152 = List(List(93.0, 89, 90), List(75.0, 76, 68), List(88.0, 82, 78))
                                                  //> cs152  : List[List[Double]] = List(List(93.0, 89.0, 90.0), List(75.0, 76.0
                                                  //| , 68.0), List(88.0, 82.0, 78.0))
       avgAvg(cs152)                              //> res86: List[Double] = List(90.66666666666667, 73.0, 82.66666666666667)
       passing(cs152)                             //> res87: List[Int] = List(0, 1, 2)
       sumSums(cs152)                             //> res88: Double = 739.0
       
       val cs153 = List(List(45.0, 90,60, 100), List(99.0, 72, 65, 0), List(88.0, 88, 88, 40), List(50.0, 100, 50, 70))
                                                  //> cs153  : List[List[Double]] = List(List(45.0, 90.0, 60.0, 100.0), List(99.
                                                  //| 0, 72.0, 65.0, 0.0), List(88.0, 88.0, 88.0, 40.0), List(50.0, 100.0, 50.0,
                                                  //|  70.0))
       avgAvg(cs153)                              //> res89: List[Double] = List(73.75, 59.0, 76.0, 67.5)
       passing(cs153)                             //> res90: List[Int] = List(0, 2)
       sumSums(cs153)                             //> res91: Double = 1105.0
       
       
       /*
        * Problem 15: Document Dictionary
        */
        def spellCheck(doc: List[String], dictionary: List[String]): List[String] = doc.filter(!dictionary.contains(_))
                                                  //> spellCheck: (doc: List[String], dictionary: List[String])List[String]
        
        val doc1 = List("Hello", "my", "name", "is", "Spencer", ".", "How", "about", "you", "?")
                                                  //> doc1  : List[String] = List(Hello, my, name, is, Spencer, ., How, about, y
                                                  //| ou, ?)
        val dict1 = List("name", "is", "you", "?", ".", "fire", "pig", "casket")
                                                  //> dict1  : List[String] = List(name, is, you, ?, ., fire, pig, casket)
        spellCheck(doc1, dict1)                   //> res92: List[String] = List(Hello, my, Spencer, How, about)
        
        val doc2 = List("Four", "score", "and", "seven", "years", "ago", "...")
                                                  //> doc2  : List[String] = List(Four, score, and, seven, years, ago, ...)
        val dict2 = List("score", "years", "...") //> dict2  : List[String] = List(score, years, ...)
        val dict3 = List("Four", "score", "and", "seven", "years", "ago", "...")
                                                  //> dict3  : List[String] = List(Four, score, and, seven, years, ago, ...)
        
        spellCheck(doc2, dict2)                   //> res93: List[String] = List(Four, and, seven, ago)
        spellCheck(doc2, dict3)                   //> res94: List[String] = List()
        
        
        
        /*
         * Problem 16: Polynomial Evaluation
         */
         def evalMono(mono: (Double, Double), x: Double): Double = mono._1 * math.pow(x, mono._2)
                                                  //> evalMono: (mono: (Double, Double), x: Double)Double
         def evalPoly(poly: List[(Double, Double)], x: Double): Double = poly.map(evalMono(_, x)).reduce(_ + _)
                                                  //> evalPoly: (poly: List[(Double, Double)], x: Double)Double
         
         evalPoly(List((1,2), (-2, 1), (8,0)), 4) //> res95: Double = 16.0
         evalPoly(List((5,5), (-4, 3), (8,2),(8,0)), 3)
                                                  //> res96: Double = 1187.0
         evalPoly(List((1, 2), (-225, 0)), 15)    //> res97: Double = 0.0
         
         
         
         /*
          * Problem 17: Accumulator
          */
          def accum(list: List[Int=>Int]) = {
          		var result = 0
          		for (f <- list)
          			result = f(result)
          		println("Register = " + result)
          }                                       //> accum: (list: List[Int => Int])Unit
          def display(x: Int): Int = {println("Register = " + x); x}
                                                  //> display: (x: Int)Int
          def clear(x: Int): Int = 0              //> clear: (x: Int)Int
          def add(x: Int): Int=>Int = (y: Int) => x + y
                                                  //> add: (x: Int)Int => Int
          def mul(x: Int): Int=>Int = (y: Int) => x * y
                                                  //> mul: (x: Int)Int => Int
          
          val program1 = List(add(3), add(2),display _, add(9), clear _, display _, add(15), mul(135))
                                                  //> program1  : List[Int => Int] = List(session$$$Lambda$91/1379435698@5e5792a
                                                  //| 0, session$$$Lambda$91/1379435698@26653222, session$$$Lambda$92/1529306539
                                                  //| @3532ec19, session$$$Lambda$91/1379435698@68c4039c, session$$$Lambda$93/16
                                                  //| 35985705@ae45eb6, session$$$Lambda$94/695682681@59f99ea, session$$$Lambda$
                                                  //| 91/1379435698@27efef64, session$$$Lambda$95/1073502961@6f7fd0e6)
          val program2 = List(add(3), mul(4), add(5), display _, clear _, add(9), mul(2))
                                                  //> program2  : List[Int => Int] = List(session$$$Lambda$91/1379435698@66a3ffe
                                                  //| c, session$$$Lambda$95/1073502961@77caeb3e, session$$$Lambda$91/1379435698
                                                  //| @1e88b3c, session$$$Lambda$96/1204167249@42d80b78, session$$$Lambda$97/104
                                                  //| 7503754@3bfdc050, session$$$Lambda$91/1379435698@1bce4f0a, session$$$Lambd
                                                  //| a$95/1073502961@5e3a8624)
          accum(program1)                         //> Register = 5
                                                  //| Register = 0
                                                  //| Register = 2025
          accum(program2)                         //> Register = 17
                                                  //| Register = 18
}