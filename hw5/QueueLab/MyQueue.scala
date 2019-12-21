package QueueLab
import scala.collection.mutable.ArrayBuffer

class MyQueue [T] {
  private var q: ArrayBuffer[T] = new ArrayBuffer[T]()
  def queue = q
  def enqueue(element: T) {q = q :+ element}
  def dequeue {q.remove(0)}
  def front: T = q.head
  def isEmpty: Boolean = q.length == 0
  override def toString: String = {
    var result = "("
    for (element <- q)
      result = result + element.toString + "," 
    result = result + ")"
    result
  }
}

object MyQueue {
  def apply[T](elems: T*) = new MyQueue[T]()
  def test1() {
     val waitingList = MyQueue[String]("Sid", "Barb", "Joel")
     waitingList.enqueue("Bob")
     waitingList.enqueue("Sally")
     waitingList.enqueue("Spencer")
     waitingList.enqueue("Gill")
     waitingList.enqueue("Samuel")
     println(waitingList.toString)
     while (!waitingList.isEmpty) {
       print("Removing " + waitingList.front + ":")
       waitingList.dequeue
       println(waitingList.toString) 
     }
  }
}

object testMyQueue extends App {
  MyQueue.test1
}