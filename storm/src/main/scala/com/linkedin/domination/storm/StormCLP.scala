package com.linkedin.domination.storm

import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Size
import CLP._ 

object StormCLP {
  
  case class PlanetState(id: Int, population: VariableNode, turn: Int, owner: Int, relativeSize: Size) {
    override def toString = "PlanetState(id=" + id + ", population=" + population +
    		", turn=" + turn + ", owner=" + owner + ", size=" + relativeSize + ")"
  }
  
  class Growth(source: PlanetState, target: PlanetState, growth: Var) extends Edge(source.population, target.population) {
    
    def calculateCurrent = source.population.current + growth
    
    def backPropagate(v: Var) = {
      source.population.set(v - growth)
    }
    
    override def toString = "Growth(source=" + source + ", target=" + target +
    		", growth=" + growth + ")"
  }

  class InitialState(target: PlanetState, value: Var) extends Edge(null, target.population) {
    
    def calculateCurrent = value
    
    def backPropagate(v: Var) = {}
    
    override def toString = "InitialState(target=" + target + ", value=" + value + ")"
  }
  
}