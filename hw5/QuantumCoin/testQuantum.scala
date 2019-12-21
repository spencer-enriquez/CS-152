package QuantumCoin

/*
 * Problem 4: Quantum Coin
 */
object testQuantum extends App {
  for (i <- 0 until 33) {
    //println("Iteration " + i)
    for(j <- 1 to 3) {
      Miner.addTransaction(j, j + 1, (math.random * 100 + 1).toInt) 
      //println("Transaction from " + Miner.currentBlock.transactions(i * 3 + (j - 1)).fromAcct + " to " + Miner.currentBlock.transactions(i * 3 + (j - 1)).toAcct + ": " + Miner.currentBlock.transactions(j).amount)
    }
  }
  // 100th Transaction means Miner will try to add to BlockChain
  Miner.addTransaction((math.random * 2 + 1).asInstanceOf[Int],(math.random * 2 + 1).asInstanceOf[Int], (math.random * 100).asInstanceOf[Int])
  println("BlockChain Account 1 Balance = " + Miner.ledger.balance(1))
  println("BlockChain Account 2 Balance = " + Miner.ledger.balance(2))
  println("BlockChain Account 3 Balance = " + Miner.ledger.balance(3))

}