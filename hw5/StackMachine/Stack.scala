package StackMachine
import scala.collection.mutable.ArrayBuffer

class Stack[T] ( val capacity: Int = 100) {
  private val elems: ArrayBuffer[T] = new ArrayBuffer[T](capacity)
  private var sp = 0
  def clear {elems.clear; sp = 0}
  def push(elem: T) {
    if (elems.size > sp) {sp += 1; elems(sp) = elem}
    else {elems += elem; sp += 1}
  }
  def pop() { 
    if (sp > 0) { 
      sp -= 1
    } 
    else println("Not enough elements to execute Pop().")
  }
  def top: Option[T] = if (sp > 0) Some(elems(sp - 1)) else None
}