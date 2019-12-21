package StackMachine

/*
 * Problem 8: Stack Machine
 */
object testStackMachine {
		//StackMachine.program = List(Push(3), Push(7), Push(18), Sum(), Top(), Pop(), Top(), Pop())
		StackMachine.program = List(Push(3), Push(7), Push(18), Top(), Pop(), Top())
		StackMachine.run()                //> Top = 18
                                                  //| Top = 7

  		StackMachine.program = List(Push(10), Push(10), Times(), Push(20), Sum(), Top())
 		//StackMachine.program
  		StackMachine.run()                //> java.lang.IndexOutOfBoundsException: 2
                                                  //| 	at scala.collection.mutable.ResizableArray.update(ResizableArray.scala:4
                                                  //| 8)
                                                  //| 	at scala.collection.mutable.ResizableArray.update$(ResizableArray.scala:
                                                  //| 47)
                                                  //| 	at scala.collection.mutable.ArrayBuffer.update(ArrayBuffer.scala:48)
                                                  //| 	at StackMachine.Stack.push(Stack.scala:9)
                                                  //| 	at StackMachine.Push.execute(Push.scala:5)
                                                  //| 	at StackMachine.StackMachine$.$anonfun$run$1(StackMachine.scala:14)
                                                  //| 	at StackMachine.StackMachine$.$anonfun$run$1$adapted(StackMachine.scala:
                                                  //| 13)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:389)
                                                  //| 	at StackMachine.StackMachine$.run(StackMachine.scala:13)
                                                  //| 	at StackMachine.testStackMachine$.$anonfun$main$1(StackMachine.testStack
                                                  //| Machine.scala:13)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at
                                                  //| Output exceeds cutoff limit.
  
  		StackMachine.program = List(Push(3), Push(4), Push(5), Sum(), Times(), Top())
  		//StackMachine.program
  		StackMachine.run()
  
  		StackMachine.program = List(Push(3), Times(), Top())
  		StackMachine.run()
  	
  		StackMachine.program = List(Top())
  		StackMachine.run()
 }