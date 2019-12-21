package test

import value._

object NumberTest1 extends App {
  var i1 = Integer(7)
  var i2 = Integer(6)
  println("i1 * i2 = " + (i1 * i2)) // i1 * i2 = 42
  println("i1 == i2 = " + (i1 == i2)) // i1 == i2 = false
  println("i1 < i2 = " + (i1 < i2)) // i1 < i2 = false
  println("i1.## = " + i1.##) // i1.## = 55
  
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("-r1 = " + -r1) // -r1 = -3.14
  println("r1 * r2 = " + (r1 * r2)) // r1 * r2 = 8.5094
  println("r1 == r2 = " + (r1 == r2)) //r1 == r2 = false
  println("r1 < r2 = " + (r1 < r2)) // r1 < r2 = false
  println("r1.## = " + r1.##) // r1.## = 1565118
  
  println("r1 * i2 = " + (r1 * i2)) // r1 * i2 = 18.84
  println("i1 * r2 = " + (i1 * r2)) // i1 * r2 = 18.97
}