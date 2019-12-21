package Recognizers

object testRecog extends App with Recognizers {
  
  // exp1 ::= 00 ~ 11 | 111
  def exp1 = pipe(follows(matches("00"), matches("11")), matches("111"))
  
  println(exp1("0011")) // = true
  println(exp1("111"))  // = true
  println(exp1("000011")) // = false, too many 0's

  // exp2 = ((00) ~ (111)* ~ (00)?) | (11111)
  def exp2 = pipe(follows(matches("00"), follows( rep(matches("111")), opt(matches("00")))), matches("11111"))
  println("\n" + exp2("00111111"))
  println(exp2("0011100"))
  println(exp2("11111"))
  println(exp2("110000011"))
}