/*
 * Problem 7: Blackjack 3.0
 */
import scala.util._
import util.control.Breaks._

object BlackJack3 extends App {
  
  val gen = new Random(System.currentTimeMillis())
  
  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1
  
  var total = 0
  
  // iterate through cards incrementing total, use break to continue and break
 
  for (j <- 0 until 52 if total < 21) {
    if (cards(j) > 0) {
      println("Drawing Card... " + cards(j))
      total = total + cards(j)
    }
  }
  
  val result = if (total == 21) "Blackjack 3.0! Total = " + total else "Bust 3.0! Total = " + total 
  println(result)
}

/*
Drawing Card... 9
Drawing Card... 5
Drawing Card... 7
Blackjack 3.0! Total = 21
* 
Drawing Card... 3
Drawing Card... 7
Drawing Card... 3
Drawing Card... 4
Drawing Card... 8
Bust 3.0! Total = 25
*
Drawing Card... 2
Drawing Card... 8
Drawing Card... 8
Drawing Card... 6
Bust 3.0! Total = 24
*/