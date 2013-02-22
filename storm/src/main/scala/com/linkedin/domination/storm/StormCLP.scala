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
    
    
  case class PlanetState(id: Int, population: Node, turn: Int, owner: Int, relativeSize: Size) {
      
    var nextTurn: Option[PlanetState] = None
    
    def addIncoming(e: Edge) = population.incoming ::= e
    
    def addOutgoing(e: Edge) = population.outgoing ::= e
    
    def set(v: Var) = {
      population.set(v)
      population.propagateBothWays
    }
    
    def createNextTurnState(size: Size): PlanetState = {
      val initialPopulation =  owner match {
        case NeutralPlanet => population.outgoing match {
          case Nil => population.current
          case x => population.current - population.calculate(x)
        }
        case _ => population.outgoing match {
          case Nil => population.current + growhValForSize(relativeSize) 
          case x => population.current + growhValForSize(relativeSize) - population.calculate(x)
        }
      }
      val nextTurn = PlanetState(id, new Node(initialPopulation, SizeRanges(size)), turn + 1, owner, size)
      val growth = owner match {
        case NeutralPlanet => None
        case _ => Some(new Growth(nextTurn, growhValForSize(relativeSize))) 
      }
      new NextTurn(this, nextTurn, growth)
      nextTurn
    }
    
    def current: Var = population.current
    
    override def toString = "PlanetState(id=" + id + ", population=" + population.current +
    		", turn=" + turn + ", owner=" + owner + ", size=" + relativeSize + ")"
  }
  
  object PlanetState {
    
    def initialPlanetState(id: Int, owner: Int, size: Size): PlanetState = owner match {
      case NeutralPlanet => {
    	val population = initialVarForSize(size)
        val planetState = PlanetState(id,
        			new Node(population, SizeRanges(size)),
        			0,
        			NeutralPlanet,
        			size)
        planetState.population.incoming ::= new Initial(planetState, population)
        planetState
      }
      case player => {
        val population = Val(40)
        val planetState = PlanetState(id,
        			new Node(population, SizeRanges(size)),
        			0,
        			player,
        			size)
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

  class Growth(target: PlanetState, value: Val) extends Edge(None, Some(target.population)) {
    
    target.addIncoming(this)
    
    def calculateCurrent = value
    
    override def backPropagate(v: Var) = set(v)
    
    override def toString = "Growth(target=" + target + ", value=" + value + ")"
  }

  class Initial(tgt: PlanetState, value: Var) extends Edge(None, Some(tgt.population)) {
    
    tgt.addIncoming(this)
    
    def calculateCurrent = value
    
    override def backPropagate(v: Var) = set(v)
    
    override def toString = "Initial(target=" + tgt + ", value=" + value + ")"
  }
  
  
  class Flight(src: PlanetState, val relativeSize: Size, val duration: Int, val owner: Int) extends Edge(Some(src.population), None) {
    
    src.addOutgoing(this)

    private var target: PlanetState = null
    
    def setTarget(t: PlanetState) = {
      tgt = Some(t.population)
      target = t
      target.addIncoming(this)
      t.set(t.population.calculate(t.population.incoming))
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
      
    val FleetSizes: List[(FleetType, Double)] =
      List((FleetType.SCOUTING, 0.25), (FleetType.RAIDING, 0.5),
           (FleetType.ASSAULT, 0.75), (FleetType.HORDE, 1))
      
    def fleetTypes: List[FleetType] = {
      for ((fs, d) <- FleetSizes if ((src.population.current * d) intersects SizeRanges(relativeSize)))
        yield fs
    }
    
    override def toString = "Flight(source=" + src + ", target=" + target +
    		", size=" + relativeSize + ", fleetTypes=" + fleetTypes +
    		", current=" + current + ", duration=" + duration + ", owner=" + owner + ")"
  }
  
}