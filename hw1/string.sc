import scala.util.Random

object String_Processing {

	/*
	 * Problem 1: Palindrome Check
	 */
  def isPal(n: String): Boolean =
  {
  		var result = true
  		for (i <- 0 to n.size / 2 if result != false) {if (n(i) != n((n.size - 1) - i)) result  = false}
  		result
  }                                               //> isPal: (n: String)Boolean
  
  isPal("rotator")                                //> res0: Boolean = true
  isPal ("Spencer")                               //> res1: Boolean = false
  isPal("yonder")                                 //> res2: Boolean = false
  isPal("racecar")                                //> res3: Boolean = true
  isPal("cat")                                    //> res4: Boolean = false
  isPal("Civic")                                  //> res5: Boolean = false
  isPal("Toyota")                                 //> res6: Boolean = false
  isPal("$3.1441.3$")                             //> res7: Boolean = true
  isPal("RaceCar")                                //> res8: Boolean = false
  
  /*
   * Problem 2: Enhanced Palindrome Check
   */
   def isPal2(n: String): Boolean =
   {
   	var phrase = ""
   	for (i <- 0 until n.size) {
   		 if (n(i).isLetterOrDigit) phrase = phrase + n(i).toLower
   	}
   	isPal(phrase)
   }                                              //> isPal2: (n: String)Boolean
   
   isPal2("roTator")                              //> res9: Boolean = true
   isPal2("yonder")                               //> res10: Boolean = false
   isPal2("racecar")                              //> res11: Boolean = true
   isPal2("A man, a plan, a canal, Panama!")      //> res12: Boolean = true
   isPal2("This sentence is definitely a palindrome.")
                                                  //> res13: Boolean = false
   isPal2("Eva, can I see bees in a cave?")       //> res14: Boolean = true
	

/*
 * Question 3: Random String Generator
 */
 def mkWord(size: Int): String =
 {
 	var string = ""
 	val r = scala.util.Random
 	for (i <- 0 until size) string = string + (r.nextInt(25) + 97).toChar
 	string
 }                                                //> mkWord: (size: Int)String
 
 mkWord(6)                                        //> res15: String = rycqnf
 mkWord(15)                                       //> res16: String = icmusrfxhdriphd
 mkWord(3)                                        //> res17: String = oyr
 mkWord(1)                                        //> res18: String = k
 mkWord(20)                                       //> res19: String = grngmeeagenvtjpsgvmt
 
 
 
 /*
  * Problem 4: Random Sentence Generator
  */
  def mkSentence(size: Int = 10): String =
  {
  		var string = ""
 		val r = scala.util.Random
 		for (i <- 0 until size) {
 			for ( j <- 0 until (r.nextInt(10) + 1))
 			{
 				if (string.size == 0) string  = string + (r.nextInt(25) + 97).toChar.toUpper else string  = string + (r.nextInt(25) + 97).toChar
 			}
 			if (i < size - 1) string = string + " " else string  = string + "."
 		}
 		string
  }                                               //> mkSentence: (size: Int)String
  
  mkSentence(5)                                   //> res20: String = Dqvkvfj dbyfbrogi puikwx o ppxtc.
  mkSentence()                                    //> res21: String = Ktqyoe nygeoov xsttdusk bd xkctbh ms yxla fejcxlbvol w fx.
  mkSentence(15)                                  //> res22: String = Hdhb ybankyayf r r bsmgoo okhcykyqvm woowo soof cmpgxpyjon 
                                                  //| fc wgcfuprhpj qsdaptwws qf rb ddhbbebcr.
  mkSentence()                                    //> res23: String = Vgqxx lrbpisfd qcjorryg hlx ejr qpix ylpo p rafdoo jingbqwb
                                                  //| b.

}