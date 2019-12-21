/*
 * Problem 7: Blackjack 1.0
 */
import scala.util._
import util.control.Breaks._

object BlackJack1 extends App {
  
  val gen = new Random(System.currentTimeMillis())
  
  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
  
  var total = 0
  
  // iterate through cards incrementing total, use break to continue and break
  breakable {
   for (j <- 0 until 52) {
  		 breakable {
        if (cards(j) <= 0) break
        println("Drawing Card... " + cards(j))
        total = total + cards(j);
      } 
     if (total >= 21) break
   }
  } //break
  
  val result = if (total == 21) "Blackjack 1.0! Total = " + total else "Bust 1.0! Total = " + total 
  println(result)
}

/*
 * Drawing Card... 5
 * Drawing Card... 3
 * Drawing Card... 7
 * Drawing Card... 10
 * Bust 1.0! Total = 25
 *
 * Drawing Card... 10
 * Drawing Card... 2
 * Drawing Card... 7
 * Drawing Card... 9
 * Bust 1.0! Total = 28
 * 
 * Drawing Card... 3
 * Drawing Card... 1
 * Drawing Card... 7
 * Drawing Card... 4
 * Drawing Card... 6
 * Blackjack 1.0! Total = 21
 * 
 */