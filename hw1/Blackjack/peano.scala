import scala.io._

class SyntaxException(gripe: String = "ERROR") extends Exception(gripe)
class MissingOperator extends SyntaxException("Missing operator!")
class NumberFormat extends SyntaxException("Illegal operand!")

/*
 * Problem 7: Peano Console  --INCOMPLETE, learn how to run application
 */
object peano extends App{

   def execute(cmmd: String): String = {
      // if cmmd is a valid expression, compute its value and return it as a string
	  if (cmmd.contains('+')) {
	    val optrPlus = cmmd.indexOf('+')
	    var op1 = 0.0
	    var op2 = 0.0
	    try {
	      	op1 = cmmd.take(optrPlus).trim.toDouble //try catch
	      	op2 = cmmd.substring(optrPlus + 1).trim.toDouble //try catch
	    }
	    catch{
	      case e: NumberFormatException => throw new NumberFormat
	    }
	    (op1 + op2).toString 
	  }
	  else if (cmmd.contains('*')) {
	    val optrMult = cmmd.indexOf('*')
	    var op1 = 0.0
	    var op2 = 0.0
	    try {
	      	op1 = cmmd.take(optrMult).trim.toDouble //try catch
	      	op2 = cmmd.substring(optrMult + 1).trim.toDouble //try catch
	    }
	    catch{
	      case e: NumberFormatException => throw new NumberFormat
	    }
	    (op1 * op2).toString 
	  }
	  else throw new MissingOperator
   }
   
   
   // read-execute-print loop
   def repl {
      // repeatedly:
      //   1. prompt user for a string
      //   2. quit if cmmd == "quit"
      //   3. ignore if cmmd == ""
      //   4. print execute(cmmd) otherwise
      //   5. handle all exceptions
      println("Welcome to Peano 1.0")
      var more = true
      while (more)
      {
        try {
          print("-> ")
          val exp = StdIn.readLine
          if (exp == "quit") more = false else {val result = execute(exp); println(result)}
        }
        catch {
          case e: MissingOperator => println(e.getMessage)
          case e: NumberFormat => println(e.getMessage)
          case e: SyntaxException => println(e.getMessage)
          case e: Throwable => {
            println(e.getMessage)
            e.printStackTrace()
            more == false
          }
        }
      } // while
			println("Bye")
   }
   
   repl
    
/*
Welcome to Peano 1.0
-> 56      *   90
5040.0
-> 3 * 5
15.0
-> 3*76
228.0
-> 89 +          89
178.0
-> 34+123
157.0
-> 746 + 56
802.0
-> four + 5
Illegal operand!
-> 90 * $56
Illegal operand!
-> 50 times 60
Missing operator!
-> 3.14 * 90
282.6
-> 4.876957 + 678596.8
678601.676957
-> 44.44 UNION 66
Missing operator!
-> bye
Missing operator!
-> quit
Bye

*/