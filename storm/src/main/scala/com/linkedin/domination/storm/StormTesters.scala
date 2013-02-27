package com.linkedin.domination.storm

import CLP._
import StormCLP._

object StormTesters {
  
  trait AccuracyTester {
    
    def currentPopulation(planetId: Int): Int
    
    var upperDiffSum: Double = 0
    var count: Long = 0
    
    def addMeasurment(planetId: Int, v: Var) = {
    	val pop = currentPopulation(planetId)
    	if (!v.intersects(Val(pop)))
    	  println("bug");
    	v match {
    	  case _: Val => {}
    	  case RangeVar(min, max) => upperDiffSum += (max - pop)
    	  case ListVar(l) => upperDiffSum += (l.last - pop)
    	}
    	count += 1
    }
    
    def reset = {
      upperDiffSum = 0
      count = 0
    }
    
    def avgUpperDiff = upperDiffSum / count
  }
  
  var accuracyTester: Option[AccuracyTester] = None

}