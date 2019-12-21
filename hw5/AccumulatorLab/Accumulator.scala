package AccumulatorLab
// Accumulator is singleton, halt and other methods are other object classes
object Accumulator {
  var register: Int = 0
  var program: List[Command] = List[Command]()
  var halt = false
  var IP = 0
  
  def clear {register = 0; IP = 0; halt = false}
  def run() {
    clear
    while (IP < program.size && halt == false) {
      program(IP).execute()
      IP = IP + 1
    }
  }
} 