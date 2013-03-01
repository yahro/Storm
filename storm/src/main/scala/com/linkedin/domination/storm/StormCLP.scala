package com.linkedin.domination.storm

import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Move.FleetType
import com.linkedin.domination.api.Size
import CLP._ 

object StormCLP {

  val InitialPlayerPopulation = 40
  val NeutralPlanet = 0
  
  
  def resolvePastBatlle(fleets: Map[Int, Var], possiblyAbandoned: Boolean, winner: Int): Var = {
    //if neutral player is not among fleets it still might happenned that planet
    //abandoned in that turn
    val maxPlayers =
      if (fleets.contains(NeutralPlanet) || !possiblyAbandoned)
        fleets.size
      else
        fleets.size + 1
        
    val minPlayers = fleets.size

    val minPenalty = fleets.filter(_._1 != winner).map(_._2 divCeil (maxPlayers - 1)).reduce(_ + _)
    val maxPenalty = fleets.filter(_._1 != winner).map(_._2).reduce(_ + _)
    
    rangeVarOrVal(minPenalty.min, maxPenalty.max)
  }
  
  def calculateNext(pop: Var, dep: Option[Var], arr: Option[Var]): Var = {
    dep match {
      case None => arr match {
        case None => pop
        case Some(w) => pop + w
      }
      case Some(v) => arr match {
        case None => pop - v
        case Some(w) => pop + w - v
      }
    }
  }
  
  def growthValForSize(sizes: List[Size], pop: Var, dep: Option[Var], arr: Option[Var], possiblyAbandoned: Boolean): Var = {
    val allSizes = sizes ::: sizesForVar(calculateNext(pop, dep, arr))
    
    val growths = allSizes.map {
      case Size.SMALL => Val(1).asInstanceOf[Var]
      case Size.MEDIUM => Val(2).asInstanceOf[Var]
      case Size.LARGE => Val(4).asInstanceOf[Var]
    }.reduce(_ or _)
    
    if (possiblyAbandoned)
      growths or Zero
    else
      growths
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
      population.propagateBothWays(MaxPropagationDepth)
    }
    
    def populationForNextTurn(size: Size, newOwner: Int, departure: Option[Var], arrivals: Option[Var],
        growthVal: Var, possiblyAbandoned: Boolean) =
          owner match {
            case NeutralPlanet => population.current
            case _ =>
              if (possiblyAbandoned)
                calculateNext(population.current, departure, arrivals) + growthVal - RangeVar(0,  1)
              else
                calculateNext(population.current, departure, arrivals) + growthVal
          }
    
    /**
     * TODO at this stage the size might not be right, because it does
     * not take into consideration departures which might happen in this turn
     */
    def createNextTurnState(size: Size, newOwner: Int, departure: Option[Var], arrivals: Option[Var],
        possiblyAbandoned: Boolean): PlanetState = {
      
      val growthVal = growthValForSize(relativeSizes, population.current, departure, arrivals, possiblyAbandoned)
      val initialPopulation = populationForNextTurn(size, newOwner, departure, arrivals, growthVal, possiblyAbandoned)
      
      nextTurn = Some(PlanetState(id, new Node(initialPopulation, SizeRanges(size)), turn + 1, newOwner, size :: Nil))
      
      val growth = newOwner match {
        case NeutralPlanet => None
        case _ =>
          Some(new Growth(nextTurn.get, growthVal)) 
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
    
    override def backPropagate(v: Var, depth: Int) = set(v)
  }

  class TakingOverPlanet(target: PlanetState, value: Var, flights: Option[List[Flight]]) extends OneWayIncoming(target, value) {
    override def toString = "TakingOverPlanet(target=" + target + ", value=" + current + 
    		", flights=" + flights + ")"
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