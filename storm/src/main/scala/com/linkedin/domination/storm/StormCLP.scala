package com.linkedin.domination.storm

import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Move.FleetType
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

  class Flight(source: PlanetState, target: PlanetState, relativeSize: Size, duration: Int) extends Edge(source.population, target.population) {
    
    def calculateCurrent =
      fleetTypes.map{
        case FleetType.SCOUTING => source.population.current * 0.25
        case FleetType.RAIDING => source.population.current * 0.5
        case FleetType.ASSAULT => source.population.current * 0.75
        case FleetType.HORDE => source.population.current
      }.reduce(_ or _)
      
    def backPropagate(v: Var) = {
      //we are using fact that each planet can launch only one fleet per turn
      if (v.isMorePreciseThan(current)) {
//        val (fixed, notFixed) = incoming.partition(_.current.fixed)
//        val fixedSum = fixed.size match {
//          case 0 => None
//          case _ => Some(fixed.map(_.current).reduce(_ + _))
//        }
//        if (notFixed.size > 0) {
//          if (notFixed.size == 1) {
//            val candidate = fixedSum match {
//              case Some(x) => current - x
//              case None => current
//            }
//            if (candidate.isMorePreciseThan(notFixed.head.current))
//              notFixed.head.backPropagate(candidate)
//          } else {
//            //try to improve approximation
//            notFixed.foreach {
//              edge =>
//                val notFixedSum = notFixed.filter(_ != edge).map(_.current).reduce(_ + _)
//                val candidate = edge.current and (current - (fixedSum match {
//                  case Some(x) => notFixedSum + x
//                  case None => notFixedSum
//                }))
//                if (candidate.isMorePreciseThan(edge.current))
//                  edge.backPropagate(candidate)
//            }
//          }
//        }
      }
    }
    
    val SizeRanges: Map[Size, Var] = Map(Size.SMALL -> RangeVar(1, 19),
        Size.MEDIUM -> RangeVar(20, 49), Size.LARGE -> RangeVar(50, Int.MaxValue))
    
    val FleetSizes: List[(FleetType, Double)] =
      List((FleetType.SCOUTING, 0.25), (FleetType.RAIDING, 0.5),
           (FleetType.ASSAULT, 0.75), (FleetType.HORDE, 1))
      
    def fleetTypes: List[FleetType] = {
      for ((fs, d) <- FleetSizes if ((source.population.current * d) intersects SizeRanges(relativeSize)))
        yield fs
    }
    
    override def toString = "InitialState(source=" + source + ", target=" + target +
    		", size=" + relativeSize + ", fleetTypes=" + fleetTypes + ", duration=" + duration + ")"
  }
  
}