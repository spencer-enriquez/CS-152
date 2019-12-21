import scala.util.Random

/*
 * MAthematics Problem 4: Roll Dice
 * In regards to the same sequence rolls, one has to set the seed of the random generator every time it runs to ensure a completley randmoized
 * roll every instance.
 */
object diceRoll {
  def rollDice(): (Int, Int) = {
  		val seed = scala.util.Random
  		val r = new scala.util.Random(seed.nextInt)
  		((scala.math.random * 6 + 1)toInt, r.nextInt(6) + 1)
  }                                               //> rollDice: ()(Int, Int)
  
  rollDice                                        //> res0: (Int, Int) = (4,3)
  rollDice                                        //> res1: (Int, Int) = (5,4)
  rollDice                                        //> res2: (Int, Int) = (5,1)
  rollDice                                        //> res3: (Int, Int) = (3,1)
  rollDice                                        //> res4: (Int, Int) = (6,1)
  rollDice                                        //> res5: (Int, Int) = (5,3)
}