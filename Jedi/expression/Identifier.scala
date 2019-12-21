package expression
import context.Environment
import value._

// Used to look up identifier keys in HashMap
case class Identifier(val name: String) extends Expression {
   override def toString = name
   //use try catch to throw correct exception
   def execute(env: Environment): Value = {
     val found = env(this)
     //println("found = " + found)
     if (found.isInstanceOf[Thunk])  {
       found.asInstanceOf[Thunk].apply
     }
     else if (found.isInstanceOf[Text]) {
       found.asInstanceOf[Text].body.execute(env)
     }
     else found
   }
}