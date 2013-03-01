package com.linkedin.domination.storm

import scala.collection._
import CLP._
import StormCLP._

object StormTesters {
  
  trait AccuracyTester {
    
    def currentPopulation(planetId: Int): Int
    
    def lastTurnFlight(from: Int, to: Int): Int
    
    def notifyInconsistentPlanet(planetId: Int, v: Var, planet: PlanetState, states: mutable.Map[Int, PlanetState])
    def notifyInconsistentFlight(from: Int, to: Int, pop: Int)
    
    var expectedDiffSum: Double = 0
    var upperDiffSum: Double = 0
    var count: Long = 0
    
    
    def addMeasurmentForPlanet(planetId: Int, v: Var, planet: PlanetState, states: mutable.Map[Int, PlanetState]) = {
    	val pop = currentPopulation(planetId)
    	if (!v.intersects(Val(pop)))
    	  notifyInconsistentPlanet(planetId, v, planet, states);
    	v match {
    	  case _: Val => {}
    	  case RangeVar(min, max) => upperDiffSum += (max - pop)
    	  case ListVar(l) => upperDiffSum += (l.last - pop)
    	}
    	expectedDiffSum += Math.abs(v.expected - pop)
    	count += 1
    }
    
    def addMeasurmentForFlight(from: Int, to: Int, v: Var) = {
      val pop = lastTurnFlight(from, to)
      if (!v.intersects(Val(pop)))
        notifyInconsistentFlight(from, to, pop)
    }
    
    def reset = {
      upperDiffSum = 0
      expectedDiffSum = 0
      count = 0
    }
    
    def avgUpperDiff = upperDiffSum / count
    def avgExpectedDiff = expectedDiffSum / count
  }
  
  var accuracyTester: Option[AccuracyTester] = None

}