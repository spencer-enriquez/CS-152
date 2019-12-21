package QuantumCoin

/*
 *  Problem 4: Quantum Coin
 */
class Transaction(val toAcct: Int, val fromAcct: Int, val amount: Double) {
  override def toString: String = "Transaction from " + fromAcct + " to " + toAcct + ": " + amount
}

object Transaction {
  def apply(to: Int, from: Int, amount: Double) = new Transaction(to, from, amount)
}