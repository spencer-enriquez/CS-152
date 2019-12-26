# Functional Programming in Scala
This assignment is meant to practice functional programming. The main topics covered within this practice set are 
functions, meta-functions, generic functions, lazy execution, and Discrete Dynamic Systems.

Within the DDS.sc file, each exercise emulates a Discrete Dynamic System, which has the following qualities:
- Current State
- Number of Cycles
- Halt Function
- Update Function

The way that this system runs is that during each cycle, the current state is updated by the updat function if the halt
function does not return the Boolean value true. 
