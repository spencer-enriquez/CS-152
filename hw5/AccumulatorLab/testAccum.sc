package AccumulatorLab

object testAccum {
  // computing ((3 * 5) + 1) * 2
	Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
	Accumulator.run()
	Accumulator.register                      //> res0: Int = 32
	// computing (((10 * 2) + 3) * 5)
	Accumulator.register = 0
	Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
	Accumulator.run()
	Accumulator.register                      //> res1: Int = 115
	  
	Accumulator.program = List(Add(10), Mul(2), Halt(), Add(3), Mul(5))
	Accumulator.run()
	Accumulator.register                      //> res2: Int = 20
	
	Accumulator.program = List(Add(20), Add(20), Mul(4), Goto(8), Halt(), Halt(), Mul(1000), Halt(), Add(1))
	Accumulator.run()
	Accumulator.register                      //> res3: Int = 161
}