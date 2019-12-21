package test

import value._

object BooleTest extends App {
  val t = Boole(true)
  val f = Boole(false)
  println("t && f = " + (t && f)) // t && f = false
  println("t || f = " + (t || f)) // t || f = true
  println("!t = " + (!t)) // !t = false
}