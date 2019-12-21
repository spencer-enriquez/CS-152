/*
 * Problem 7: Blackjack 2.0
 */
import scala.util._
import util.control.Breaks._

object BlackJack2 extends App {
  
  val gen = new Random(System.currentTimeMillis())
  
  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
  
  var total = 0
  
  // iterate through cards incrementing total, use break to continue and break
  try {
   for (j <- 0 until 52) {
  		 try {
        if (cards(j) <= 0) throw new Exception
        println("Drawing Card... " + cards(j))
        total = total + cards(j);
      }
  		 catch {case _: Throwable => }
     if (total >= 21) throw new Exception
   }
  } // try
  catch {
    case e: Exception => if (total == 21) println("Blackjack 2.0! Total = " + total) else println("Bust 2.0! Total = " + total)
  }
}

/*
 * Drawing Card... 2
Drawing Card... 3
Drawing Card... 5
Drawing Card... 2
Drawing Card... 5
Drawing Card... 6
Bust 2.0! Total = 23
*
Drawing Card... 10
Drawing Card... 10
Drawing Card... 1
Blackjack 2.0! Total = 21
* 
Drawing Card... 3
Drawing Card... 10
Drawing Card... 10
Bust 2.0! Total = 23
*/