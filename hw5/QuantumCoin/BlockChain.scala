package QuantumCoin
import scala.collection.mutable.ArrayBuffer

/*
 *  Problem 4: Quantum Coin
 */
class BlockChain {
  val control = 1000000000
  private var chain = ArrayBuffer[Block]()
  
  def getChain = chain
  //sum of each account's balance in each block
  def balance(acct: Int): Double = {
    var sum = 0.0
    for (b <- chain)
      sum = sum + b.balance(acct)
    sum
  }
  def add(b: Block) {
    if (b.hashCode < control) chain = chain :+ b 
    else throw new Exception("Unable to add Block: " + b.hashCode + " is too low.")
  }
}

object Miner {
  var currentBlock: Block = new Block
  val ledger: BlockChain = new BlockChain
  def addTransaction(fromAcct: Int, toAcct: Int, amt: Double) {
    val t = Transaction(toAcct, fromAcct, amt)
    if (currentBlock.transactions.size + 1 < 100) currentBlock.add(t)
    else if (currentBlock.transactions.size + 1 == 100) {
      currentBlock.add(t)
      println("Block Account 1 Balance = " + currentBlock.balance(1))
      println("Block Account 2 Balance = " + currentBlock.balance(2))
      println("Block Account 3 Balance = " + currentBlock.balance(3))
      println("Block HashCode before add to BlockChain = " + currentBlock.hashCode())
      var blockAdded = false
      val chainBefore = ledger.getChain.size
      for (i <- 0 until 50 if blockAdded == false) {
        try {
          ledger.add(currentBlock)
          if (ledger.getChain.size > chainBefore){
            blockAdded = true
            println("Current Block has been added at HashCode " + currentBlock.hashCode)
          }
        }
        catch {
          case e: Exception => println(e); currentBlock.incSconce
        }
      }
      currentBlock = new Block
    }
  }
}
