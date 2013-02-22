package com.linkedin.domination.storm

import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Move.FleetType
import com.linkedin.domination.api.Size
import CLP._ 

object StormCLP {

  val InitialPlayerPopulation = 40
  val NeutralPlanet = 0
  
  def growhValForSize(size: Size): Val = size match {
    case Size.SMALL => Val(1)
    case Size.MEDIUM => Val(2)
    case Size.LARGE => Val(4)
  }

  val SizeRanges: Map[Size, RangeVar] = Map(Size.SMALL -> RangeVar(1, 19),
    Size.MEDIUM -> RangeVar(20, 49), Size.LARGE -> RangeVar(50, Int.MaxValue))
    
  def initialVarForSize(size: Size) =
    if (size == Size.SMALL)
      RangeVar(1, 19)
    else if (size == Size.MEDIUM)
      RangeVar(20, 49)
    else RangeVar(50, 90)
  
  def sizeForVal(v: Val) =
    if (v.v < 20) Size.SMALL
    else if (v.v < 50) Size.MEDIUM
    else Size.LARGE
    
    
  case class PlanetState(id: Int, population: VariableNode, turn: Int, owner: Int, relativeSize: Size) {
      
    var nextTurn: Option[PlanetState] = None
    
    def addIncoming(e: Edge) = population.incoming ::= e
    
    def addOutgoing(e: Edge) = population.outgoing ::= e
    
    def createNextTurnState(size: Size): PlanetState = {
      val nextTurn = PlanetState(id, new VariableNode(population.current + growhValForSize(relativeSize), SizeRanges(size)), turn + 1, owner, size)
      val growth = owner match {
        case NeutralPlanet => None
        case _ => Some(new Growth(nextTurn, growhValForSize(relativeSize))) 
      }
      new NextTurn(this, nextTurn, growth)
      nextTurn.population.recalculate
      nextTurn
    }
    
    def set(v: Var): Boolean = population.set(v)
    def current: Var = population.current
    
    override def toString = "PlanetState(id=" + id + ", population=" + population.current +
    		", turn=" + turn + ", owner=" + owner + ", size=" + relativeSize + ")"
  }
  
  object PlanetState {
    
    def initialPlanetState(id: Int, owner: Int, size: Size): PlanetState = owner match {
      case NeutralPlanet => {
    	val population = initialVarForSize(size)
        val planetState = PlanetState(id,
        			new VariableNode(population, SizeRanges(size)),
        			0,
        			NeutralPlanet,
        			size)
        planetState.population.incoming ::= new Initial(planetState, population)
        planetState
      }
      case player => {
        val population = Val(40)
        val planetState = PlanetState(id,
        			new VariableNode(population, SizeRanges(size)),
        			0,
        			player,
        			size)
        planetState.population.incoming ::= new Initial(planetState, population)
        planetState
      }
    }
    
  }
  
  
  class NextTurn(source: PlanetState, target: PlanetState, growth: Option[Growth]) extends Edge(source.population, Some(target.population)) {
    
    source.addOutgoing(this)
    target.addIncoming(this)
    
    def calculateCurrent = {
      val outgoingSum = source.population.outgoing.filter(_ != this) match {
        case Nil => None
        case l => Some(l.map(_.current).reduce(_ + _))
      }
      outgoingSum match {
        case None => source.population.current
        case Some(s) => source.population.current - s
      }
    }
    
    def backPropagate(v: Var) = {
      set(v)
      source.population.recalculate
    }
//      source.population.set(growth match { case None => v; case Some(g) => v - g.current})
    
    override def toString = "NextTurn(source=" + source + ", target=" + target + 
    		", growth=" + growth + ")"
  }

  class Growth(target: PlanetState, value: Val) extends Edge(null, Some(target.population)) {
    
    target.addIncoming(this)
    
    def calculateCurrent = value
    
    def backPropagate(v: Var) = {}
    
    override def toString = "Growth(target=" + target + ", value=" + value + ")"
  }

  class Initial(tgt: PlanetState, value: Var) extends Edge(null, Some(tgt.population)) {
    
    tgt.addIncoming(this)
    
    def calculateCurrent = value
    
    def backPropagate(v: Var) = {}
    
    override def toString = "Initial(target=" + tgt + ", value=" + value + ")"
  }
  
  
  class Flight(src: PlanetState, relativeSize: Size, duration: Int) extends Edge(src.population, None) {
    
    src.addOutgoing(this)

    private var tgt: PlanetState = null
    
    def setTarget(t: PlanetState) = {
      target = Some(t.population)
      tgt = t
      tgt.addIncoming(this)
      tgt.population.recalculate
    }
    
    def calculateCurrent =
      fleetTypes.filter{
      //filter out HORDE in case planet was not abandoned
        ft =>
          (ft != FleetType.HORDE) ||
          	(src.nextTurn.getOrElse(src).owner == NeutralPlanet)
      }.map{
        case FleetType.SCOUTING => src.population.current * 0.25
        case FleetType.RAIDING => src.population.current * 0.5
        case FleetType.ASSAULT => src.population.current * 0.75
        case FleetType.HORDE => src.population.current
      }.reduce(_ or _)
      
    def backPropagate(v: Var) = {
      //we are using fact that each planet can launch only one fleet per turn
//      if (v.isMorePreciseThan(current)) {
//        val (fixed, notFixed) = src.population.outgoing.partition(_.current.fixed)
//        val fixedSum = fixed.size match {
//          case 0 => None
//          case _ => Some(fixed.map(_.current).reduce(_ + _))
//        }
//        if (notFixed.size > 0) {
//          if (notFixed.size == 1) {
//            val candidate = fixedSum match {
//              case Some(x) => src.population.current - x
//              case None => src.population.current
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
//      }
    }
    
    val FleetSizes: List[(FleetType, Double)] =
      List((FleetType.SCOUTING, 0.25), (FleetType.RAIDING, 0.5),
           (FleetType.ASSAULT, 0.75), (FleetType.HORDE, 1))
      
    def fleetTypes: List[FleetType] = {
      for ((fs, d) <- FleetSizes if ((src.population.current * d) intersects SizeRanges(relativeSize)))
        yield fs
    }
    
    override def toString = "InitialState(source=" + src + ", target=" + tgt +
    		", size=" + relativeSize + ", fleetTypes=" + fleetTypes + ", duration=" + duration + ")"
  }
  
}