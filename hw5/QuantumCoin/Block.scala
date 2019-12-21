package QuantumCoin
import scala.collection.mutable.ArrayBuffer

/*
 *  Problem 4: Quantum Coin
 */
class Block {
  private var sconce = 0
  private val trans = new ArrayBuffer[Transaction]()
  
  
  def transactions = trans
  def add(t: Transaction) {trans += t}
  def getSconce = sconce
  def incSconce {sconce = sconce + 1}
  
  def balance(acct: Int): Double = {
    var sum = 0.0
    for (t <- trans) {
      if (t.toAcct == acct) sum = sum + t.amount
      else if (t.fromAcct == acct) sum = sum - t.amount
    }
    sum  
  }
  override def hashCode(): Int = toString.hashCode()
  override def toString(): String = {
    var result = "Sconce = " + sconce + "\n" + "{"
    for (t <- trans)
       result = result  + t.toString
    result
  }
}

object Block {
  def apply = new Block()
}

object test extends App{
  val b = Block
  println(b.hashCode())
}