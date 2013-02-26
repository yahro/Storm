package com.linkedin.domination.storm

import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Move.FleetType
import com.linkedin.domination.api.Size
import CLP._ 

object StormCLP {

  val InitialPlayerPopulation = 40
  val NeutralPlanet = 0
  
  def growthValForSize(sizes: List[Size], pop: Var, dep: Option[Var], arr: Option[Var]): Var = {
    val allSizes = sizes ::: (dep match {
      case None => arr match {
        case None => sizesForVar(pop)
        case Some(w) => sizesForVar(pop + w)
      }
      case Some(v) => arr match {
        case None => sizesForVar(pop - v)
        case Some(w) => sizesForVar(pop + w - v)
      }
    })
    allSizes.map {
      case Size.SMALL => Val(1).asInstanceOf[Var]
      case Size.MEDIUM => Val(2).asInstanceOf[Var]
      case Size.LARGE => Val(4).asInstanceOf[Var]
    }.reduce(_ or _)
  }

  val FleetSizes: List[(FleetType, Double)] =
    List((FleetType.SCOUTING, 0.25), (FleetType.RAIDING, 0.5),
      (FleetType.ASSAULT, 0.75), (FleetType.HORDE, 1))
  
  val SizeRanges: Map[Size, RangeVar] = Map(Size.SMALL -> RangeVar(0, 19),
    Size.MEDIUM -> RangeVar(20, 49), Size.LARGE -> RangeVar(50, Int.MaxValue))
    
  def sizesForVar(v: Var): List[Size] =
    (for {
      (size, range) <- SizeRanges
      if (v.intersects(range))	
    } yield size).toList
    
  def initialVarForSize(size: Size) =
    if (size == Size.SMALL)
      RangeVar(0, 19)
    else if (size == Size.MEDIUM)
      RangeVar(20, 49)
    else RangeVar(50, 90)
  
  def sizeForVal(v: Val) = Size.getSizeForNumber(v.v)
    
    
  case class PlanetState(id: Int, population: Node, turn: Int, owner: Int, relativeSizes: List[Size]) {
      
    var nextTurn: Option[PlanetState] = None
    
    def addIncoming(e: Edge) = population.incoming ::= e
    
    def addOutgoing(e: Edge) = population.outgoing ::= e
    
    def set(v: Var) = {
      population.set(v)
      population.propagateBothWays
    }
    
    def populationForNextTurn(size: Size, newOwner: Int, departure: Option[Var], arrivals: Option[Var]) = owner match {
        case NeutralPlanet => population.outgoing match {
          case Nil => population.current
          case x => population.current - x.map(_.current).reduce(_ + _)
        }
        case _ => population.outgoing match {
          case Nil => population.current + growthValForSize(relativeSizes, population.current, departure, arrivals)
          case x => {
            //take care of situation, where planet gets abandoned
            val calculated = x.map(_.current).reduce(_ + _)
            if (newOwner == NeutralPlanet)
              population.current - calculated
            else
              //here we know that planet is not neutral, which means that calculated population
              //can not be 0, hence and with RangeVar(1, Int.MaxValue)
              ((population.current - calculated) and RangeVar(1, Int.MaxValue)) +
              	growthValForSize(relativeSizes, population.current, departure, arrivals)
          }
        }
      }
    
    /**
     * TODO at this stage the size might not be right, because it does
     * not take into consideration departures which might happen in this turn
     */
    def createNextTurnState(size: Size, newOwner: Int, departure: Option[Var], arrivals: Option[Var]): PlanetState = {
      val initialPopulation = populationForNextTurn(size, newOwner, departure, arrivals)
      nextTurn = Some(PlanetState(id, new Node(initialPopulation, SizeRanges(size)), turn + 1, newOwner, size :: Nil))
      val growth = newOwner match {
        case NeutralPlanet => None
        case _ =>
          Some(new Growth(nextTurn.get, growthValForSize(relativeSizes, population.current, departure, arrivals))) 
      }
      new NextTurn(this, nextTurn.get, growth)
      nextTurn.get
    }
    
    def current: Var = population.current
    
    override def toString = "PlanetState(id=" + id + ", population=" + population.current +
    		", turn=" + turn + ", owner=" + owner + ", size=" + relativeSizes + ")"
  }
  
  object PlanetState {
    
    def initialPlanetState(id: Int, owner: Int, size: Size): PlanetState = owner match {
      case NeutralPlanet => {
    	val population = initialVarForSize(size)
        val planetState = PlanetState(id,
        			new Node(population, SizeRanges(size)),
        			0,
        			NeutralPlanet,
        			size :: Nil)
        planetState.population.incoming ::= new Initial(planetState, population)
        planetState
      }
      case player => {
        val population = Val(40)
        val planetState = PlanetState(id,
        			new Node(population, SizeRanges(size)),
        			0,
        			player,
        			size :: Nil)
        planetState.population.incoming ::= new Initial(planetState, population)
        planetState
      }
    }
    
  }
  
  
  class NextTurn(source: PlanetState, target: PlanetState, growth: Option[Growth]) extends Edge(Some(source.population), Some(target.population)) {
    
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
    
    override def toString = "NextTurn(source=" + source + ", target=" + target + 
    		", growth=" + growth + ", state=" + state + ")"
  }

  class BeforeCombat(source: PlanetState) extends Edge(Some(source.population), None) {
    
    source.addOutgoing(this)
    
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
    
    override def toString = "BeforeCombat(source=" + source + ", state=" + state + ")"
  }
  
  abstract class OneWayIncoming(target: PlanetState, value: Var) extends Edge(None, Some(target.population)) {
    
    target.addIncoming(this)
    
    def calculateCurrent = value
    
    override def backPropagate(v: Var) = set(v)
  }

  class Growth(target: PlanetState, value: Var) extends OneWayIncoming(target, value) {
    override def toString = "Growth(target=" + target + ", value=" + value + ")"
  }

  class Initial(tgt: PlanetState, value: Var) extends OneWayIncoming(tgt, value) {
    override def toString = "Initial(target=" + tgt + ", value=" + value + ")"
  }
  
  class FromCombat(tgt: PlanetState, value: Var,
      val flights: List[Flight], val planet: PlanetState) extends OneWayIncoming(tgt, value) {
    override def toString = "FromCombat(target=" + tgt + ", value=" + value + ")"
  }
  
  class Flight(val source: PlanetState, val relativeSize: Size, val duration: Int, val owner: Int, destination: Int) extends Edge(Some(source.population), None) {
    
    source.addOutgoing(this)

    private var target: PlanetState = null
    
    def setTarget(t: PlanetState) = {
      tgt = Some(t.population)
      target = t
      target.addIncoming(this)
    }
    
    def calculateCurrent =
      fleetTypes.filter{
      //filter out HORDE in case planet was not abandoned
        ft =>
          (ft != FleetType.HORDE) ||
          	(source.nextTurn match {
          	  case None => true
          	  case Some(state) => state.owner == NeutralPlanet
          	})
      }.map{
        case FleetType.SCOUTING => source.population.current * 0.25
        case FleetType.RAIDING => source.population.current * 0.5
        case FleetType.ASSAULT => source.population.current * 0.75
        case FleetType.HORDE => source.population.current
      }.reduce(_ or _)
      
    def fleetTypes: List[FleetType] = {
      for ((fs, d) <- FleetSizes if ((source.population.current * d) intersects SizeRanges(relativeSize)))
        yield fs
    }
    
    override def toString = "Flight(source=" + source + ", target=" + target +
    		", size=" + relativeSize + ", fleetTypes=" + fleetTypes +
    		", current=" + current + ", duration=" + duration + ", owner=" + owner + ")"
  }
  
}