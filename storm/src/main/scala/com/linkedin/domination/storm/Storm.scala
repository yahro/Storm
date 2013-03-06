package com.linkedin.domination.storm

import com.linkedin.domination.api.Player
import com.linkedin.domination.api.Universe
import com.linkedin.domination.api.Move
import com.linkedin.domination.api.Event
import com.linkedin.domination.api.Event.EventType
import scala.collection.JavaConversions._
import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Size
import scala.collection._
import CLP._
import StormCLP._
import StormTesters._
import com.linkedin.domination.sample.WoodsmanPlayer
import com.linkedin.domination.sample.Lion
import scala.io._
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import com.linkedin.domination.api.Move.FleetType
import scala.collection.mutable.ListBuffer

/**
 * Lista problemow:
 * - zle liczone walka gdy sa reinforcements
 */

class Storm extends Player {

  val getPlayerName = "Storm"
    
  val delegate = new Lion 
    
  var playerNumber = 0
  
//  val output= new PrintWriter("/Users/jodzga/git/storm/storm/visualizer/replay/addon.json")
//  output.write("{ \"data\": [\n")
  
  def initialize(playerNbr: java.lang.Integer) = {
    playerNumber = playerNbr
    delegate.initialize(playerNbr)
  }
  
  // ** AI constants **
  val MovesAhead = 48
  
  // ** Game state **
  var turn = 0
  var numberOfPlanets = 0;
  var planetDistances: Map[(Int, Int), Int] = null
  var planetsByDistance: Map[Int, List[(Int, Int)]] = null
  
  
  val MaxCachedPopForStream = 75
  //pre-calculated list of semi-optimal stream
  //max population is 75 
  //(distance, population, turn) -> (turn -> arrival)
  val optimalStream: Map[(Int, Int, Int), Map[Int, Int]] = {
    val condidates = List((2, 1), (28, 20), (70, 50))
    val simulation =
      for {
        distance <- 1 to 65
        population <- 1 to MaxCachedPopForStream
        t <- 0 to MovesAhead
      } yield {
        
        @tailrec
        def accumulate(trn: Int, current: Int, min: Int, max: Int, deps: mutable.Map[Int, Int]): Unit = {
          if (trn <= MovesAhead && (trn + (distance - 1) <= MovesAhead)) {
            if (current > min) {
              val possibleFleets =
                for {
                  (fs, d) <- FleetSizes if (fs != FleetType.HORDE) //we don't want to abandon planet
                  pop = (current * d).toInt
                  if (current - pop >= min)
                } yield pop

              possibleFleets match {
                case Nil => accumulate(trn + 1, current + growth(1, current), min, max, deps)
                case fleets =>
                  val fmax = fleets.max
                  deps(trn) = fmax
                  accumulate(trn + 1, current - fmax + growth(1, current - fmax), min, max, deps)
              }
            } else
              accumulate(trn + 1, current + growth(1, current), min, max, deps)
          }
        }
        
        val simulations =
          for (candidate <- condidates)
            yield {
              val mp = mutable.Map[Int, Int]()
              accumulate(t, population, candidate._2, candidate._1, mp)
              (distance, population, t, mp)
            }
        
        val scored = simulations.map {
          x =>
            val score = x._4.map(_._2).sum
            (score, x)
        }
        
        val sorted = scored.sortBy(_._1).reverse
        
        (distance, population, t) -> sorted.head._2._4
      }
      
    simulation.toMap
  }
  
  def optimalStreamUnbound(distance: Int, population: Int, t: Int): Map[Int, Int] = {

    @tailrec
    def accumulate(trn: Int, current: Int, deps: mutable.Map[Int, Int]): Unit = {
      if (trn <= MovesAhead && (trn + (distance - 1) <= MovesAhead)) {
          val possibleFleets =
            for {
              (fs, d) <- FleetSizes if (fs != FleetType.HORDE) //we don't want to abandon planet
              pop = (current * d).toInt
              if (current - pop >= 50)
            } yield pop

          possibleFleets match {
            case Nil => {}
            case fleets =>
              val fmax = fleets.max
              deps(trn) = fmax
              if (current - fmax + growth(1, current - fmax) > MaxCachedPopForStream)
                accumulate(trn + 1, current - fmax + growth(1, current - fmax), deps)
              else
                if (trn < MovesAhead)
                  deps ++= optimalStream(distance, current - fmax + growth(1, current - fmax), trn + 1)
          }
      }
    }
     
    if (population <= MaxCachedPopForStream)
      optimalStream(distance, population, t)
    else {
      val mp = mutable.Map[Int, Int]()
      accumulate(t, population, mp)
      mp
    }
    
  }
  
  case class Model(timeline: mutable.Map[Int, mutable.Map[Int, PlanetState]], arrivals: mutable.Map[Int, mutable.Map[Int, List[Flight]]])
  
  /**
   * score = accumulated growth of me - accumulated growth of opponents (based on futures)
   */
  
  
  
  val model: Model = Model(mutable.Map(), mutable.Map()) //initially empty model
  
  var timing: Long = System.currentTimeMillis()
  
  def makeMove(universe: Universe, events: java.util.List[Event]): java.util.List[Move] = {  
      
    //update model past
    if (model.timeline.isEmpty)
      initializeModel(universe)
    else
      updateModel(universe, events.toList);
      
    //writeOutEstimates(output)

    //calculate future
    val arrivals = getArrivals
    val futureBase = generateFuture(arrivals)
    val balance = getBalance(arrivals, futureBase)
    val targets = getTargets(arrivals, futureBase, balance)
    
    //calculate moves
    val partialMoves = getPartialMoves(targets, balance, futureBase)
    
    validatePartialMoves(partialMoves, futureBase)
    
    //go parallel 
    val scoredMoves = partialMoves.par.map {
      move => (score(move, futureBase, arrivals), move)
    }
    
    val baseline = score(TargetedMove(0, Nil), futureBase, arrivals)
    
    val sortedByScore = scoredMoves.filter(_._1 > baseline).toList.sortBy(_._1).reverse
    
    //TODO combine moves
    val combined = combineMoves(sortedByScore)
    
    //update turn
    turn += 1
    
//    val newTime = System.currentTimeMillis()
//    println("turn: " + turn + ", moves: " + sortedByScore.size + ", time: " + (newTime - timing))
//    timing = newTime
      
    //return moves
    combined.map(m => new Move(m.from, m.to, m.fleetType))
  }
  
  def combineMoves(moves: List[(Double, TargetedMove)]): List[FFlight] = {
    val usedPlanets = mutable.Set[Int]()
    val flights =
      for {
        (score, TargetedMove(target, fs)) <- moves
        f <- fs if (!usedPlanets.contains(f.from) && f.turnDepart == 0)
      } yield {
        usedPlanets += f.from
        f
      }
    flights
  }
  
  def validatePartialMoves(partialMoves: List[TargetedMove],
      population: mutable.Map[Int, mutable.Map[Int, FPlanet]]) = {
    partialMoves.foreach {
      move => move.flights.foreach {
        flight => if (flight.turnDepart == 0) {
          val s = population(flight.from)(-1).size
          if (flight.size > s + growth(playerNumber, s) - 1) {
            val p = population(flight.from)
            val tp = p(-1)
            val x = model.timeline(flight.from)(turn)
            println("bug")
          }
        }
      }
    }
  }

  def writeOutEstimates(output: PrintWriter) = {
    output.write("  {\n")

    for ((planetId, states) <- model.timeline) {
      val cur = states(turn)
      output.write("    \"" + planetId + "\": \"" + {
        cur.current match {
          case Val(x) => x.toString
          case _ => cur.current.min.toString + "-" + cur.current.max.toString
        }
      } +
        "\",\n")
    }
    output.write("  },\n")
    output.flush()
  }
  
  /*
   * -------------- future model -------------
   */
  
  //TODO keep track scheduled fleets 
  
  case class FFleet(owner: Int, size: Int)
  
  case class FPlanet(owner: Int, size: Int)
  
  case class FFlight(from: Int, to: Int, size: Int, fleetType: FleetType, turnDepart: Int, turnArrive: Int) {
    override def toString = "from: " + from + ", to: " + to + ", size: " + size + ", depart: " + turnDepart + ", arrive: " + turnArrive 
  }
  
  case class TargetedMove(target: Int, flights: List[FFlight])
  
  def score(move: TargetedMove,
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]],
      arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]]): Double = {
    val targets = (move.flights.map(_.to) ++ arrivals.keys).toSet
    val scores = for {
      (p, turns) <- population
      state = turns(-1)
      if (state.owner != NeutralPlanet || targets.contains(p))
    } yield (p, evaluate(p, move, population, arrivals))
    scores.map(_._2).sum
  }
  
  def appendArrivals(dest: mutable.Map[Int, mutable.Map[Int,ListBuffer[FFleet]]],
      ars: mutable.Map[Int, mutable.Map[Int,ListBuffer[FFleet]]]) = {
    for {
      (t, mp) <- ars
      dests = dest.getOrElseUpdate(t, mutable.Map[Int,ListBuffer[FFleet]]())
      (p, l) <- mp
      destL = dests.get(p)
    } {
      destL match {
        case None => dests(p) = l
        case Some(dl) => dl ++= l
      }
    }
  }
  
  def evaluate(planet: Int, move: TargetedMove,
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]],
      arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]]): Double = {
    
    //populate arrivals and departures
    val latestArrival = ArrayBuffer.fill(numberOfPlanets)(-1)
    val allArrivals = mutable.Map[Int, mutable.Map[Int, ListBuffer[FFleet]]]()
    val allDepartures = mutable.Map[Int, mutable.Map[Int,FFleet]]()
    for (flight <- move.flights) {
      val deps = allDepartures.getOrElseUpdate(flight.turnDepart, mutable.Map[Int,FFleet]())
      val ars = allArrivals.getOrElseUpdate(flight.turnArrive, mutable.Map[Int,ListBuffer[FFleet]]())
      deps.put(flight.from, FFleet(playerNumber, flight.size))
      val existing = ars.get(flight.to)
      existing match {
        case None => ars.put(flight.to, ListBuffer(FFleet(playerNumber, flight.size)))
        case Some(l) => l += FFleet(playerNumber, flight.size)
      }
      if (latestArrival(flight.to) < flight.turnArrive)
        latestArrival(flight.to) = flight.turnArrive
    }

    val arrivlsFromGame = arrivals.foreach { 
      x =>
        val (p, mp) = x
        mp.foreach {
          y =>
            val (t, l) = y
            val ars = allArrivals.getOrElseUpdate(t, mutable.Map[Int,ListBuffer[FFleet]]())
            val arl = ars.get(p)
            arl match {
              case None => ars(p) = ListBuffer[FFleet]() ++= l
              case Some(el) => el ++= l
            }
            if (latestArrival(p) < t)
              latestArrival(p) = t
        }
    }
    
    val states =
      for ((p, turns) <- population)
        yield (p, turns(-1))
        
    var planetsToProcess = mutable.Set[Int](planet) ++=
      states.filter( x => x._2.owner != NeutralPlanet || latestArrival(x._1) >= 0).map(_._1).toSet
    
    @tailrec
    def calculateTurn(trn: Int, sts: Map[Int, FPlanet]): Double = {
      if (trn <= MovesAhead) {
        val nextTurnStates =
          for ((p, state) <- sts) yield {
            val ars = allArrivals.get(trn).flatMap(_.get(p).map(_.toList))
            val dep = allDepartures.get(trn).flatMap(_.get(p))
            val next = resolveFPlanet(state, ars, dep)  //take into consideration departure
            if (p != planet && latestArrival(p) <= trn) {
              if (next.owner != NeutralPlanet) {
                //add streamed fleets
                val streamFleets = optimalStreamUnbound(planetDistances(p, planet), next.size, trn)
                for ((st, ss) <- streamFleets) {
                  val arrivalTurn = st + planetDistances(p, planet) - 1
                  val arsPlanet = allArrivals.getOrElseUpdate(arrivalTurn, mutable.Map[Int, ListBuffer[FFleet]]())
                  val arP = arsPlanet.getOrElseUpdate(planet, ListBuffer[FFleet]())
                  arP += FFleet(next.owner, ss)
                }
              }
              //remove planet from further processing
              planetsToProcess -= p
            }
            (p, next)
          }
        calculateTurn(trn + 1, nextTurnStates.filterKeys(planetsToProcess.contains(_)))
      } else {
        val last = sts(planet)
        if (last.owner == playerNumber)
          last.size
        else if (last.owner != NeutralPlanet)
          -last.size
        else
          0
      }
    }
    
    calculateTurn(0, states.filterKeys(planetsToProcess.contains(_)))
  }
  
  def getPartialMoves(targets: mutable.Map[Int, mutable.Map[Int, Int]],
      balances: mutable.Map[Int, mutable.Map[Int, FPlanet]],
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]]): List[TargetedMove] = {

    def partialMovesForPlanet(t: Int, planet: Int, turnTargets: mutable.Map[Int,Int]): Option[TargetedMove] = {

	  @tailrec
	  def findTragetedMove(tgt: Int, planets: List[(Int, Int)], soFar: List[FFlight]): List[FFlight] = {
	    if (tgt <= 0)
	      soFar
	    else if (planets.isEmpty)
	      Nil
	    else {
	      val (p, dist) = planets.head
	      val pt = t - (dist - 1)
	      if (pt >= 0) {
	        //TODO is there a need to consider earlier balances? 
	        val b = balances(p)(pt)
	        if (b.owner == playerNumber && b.size > 0) {
	          
	          val possibleFleets =
	            for {
	              (fs, d) <- FleetSizes if (fs != FleetType.HORDE)  //we don't want to abandon planet
	              ppop = population(p)(pt - 1)
	              if (ppop.owner == playerNumber)  //this should be true
	              pop = ((ppop.size + growth(ppop.owner, ppop.size)) * d).toInt
	              if (pop < b.size)
	            }
                  yield (fs, pop)
                  
              possibleFleets match {
                case Nil => findTragetedMove(tgt, planets.tail, soFar)
                case x => {
                  val satisfying = x.filter(_._2 >= tgt)
                  //best fit is smallest value which fulfills target
                  //or largest possible
                  val bestFit = satisfying match {
                    case Nil => x.maxBy(_._2)
                    case y => y.minBy(_._2)
                  }
                  findTragetedMove(tgt - bestFit._2,
                      planets.tail,
                      FFlight(p, planet, bestFit._2, bestFit._1, pt, t) :: soFar)
                }
              }
	        } else
	          findTragetedMove(tgt, planets.tail, soFar)
	      } else
	        findTragetedMove(tgt, planets.tail, soFar)
	    }
	  }
      
      if (t < 0)
        None
      else {
        //
        partialMovesForPlanet(t - 1, planet, turnTargets) match {
          case x: Some[TargetedMove] => x
          case None => {
            turnTargets.get(t) match {
              case None => None
              case Some(tgt) =>
                findTragetedMove(tgt, planetsByDistance(planet), Nil) match {
                  case Nil => None
                  case x => Some(TargetedMove(planet, x))
                }
            }
          }
        }
      }
    }
    
    //for now constraint is that we consider only one flight from every planet
    val partialMoves =
      for ((planetId, turnTargets) <- targets)
        yield partialMovesForPlanet(MovesAhead, planetId, turnTargets)
      
    partialMoves.foldLeft(List[TargetedMove]()) {
      case (l, Some(e)) => e :: l
      case (l, None) => l
    }
  }
  
  /**
   * Returns possible targets (how many ships need to be sent there)
   * for attack and defense actions.
   */
  def getTargets(arrivals: mutable.Map[Int, mutable.Map[Int, List[FFleet]]],
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]],
      balance: mutable.Map[Int, mutable.Map[Int, FPlanet]]): mutable.Map[Int, mutable.Map[Int, Int]] = {

    val target = mutable.Map[Int, mutable.Map[Int, Int]]()
    for ((planetId, states) <- model.timeline) {
      val planetArrivals = arrivals.getOrElse(planetId, mutable.Map[Int, List[FFleet]]())
      val targetTurns = mutable.Map[Int, Int]()
      target(planetId) = targetTurns
      val balanceTurns = balance(planetId)
      val currentState = states(turn)
      val populationTurns = population(planetId)
      
      for (t <- 0 to 12) {
        val turnArrivals = planetArrivals.get(turn + t + 1)
        val balance = balanceTurns(t)
        if (balance.owner == playerNumber && balance.size < 0) {
          //defense
          targetTurns(t) = -balance.size
        } else {
          //attack
          val prev = populationTurns(t - 1)

          val planetFleet = FFleet(prev.owner, prev.size + growth(prev.owner, prev.size))
          
          val fleets = turnArrivals match {
            case None => List(planetFleet)
            case Some(ars) => planetFleet :: ars
          }
          val grouped = fleets.groupBy(_.owner)

          val forces = (for (force <- grouped)
            yield (FFleet(force._1, force._2.map(_.size).sum))).toList

          val maxOpponent: Int =
            forces.filter(_.owner != playerNumber) match {
              case Nil => 0
              case l =>
                l.reduce((x, y) => if (x.size < y.size) x else y).size + 1 //+1 because we need to win
            }
         
          val forcesToTakePlanet = maxOpponent - 
            fleets.find(_.owner == playerNumber).map(_.size).getOrElse(0)
            
          targetTurns(t) = forcesToTakePlanet
        }
      }
    }
    
    target
  }
  
  def growth(owner: Int, pop: Int): Int =
    if (owner == NeutralPlanet)
      0
    else
      if (pop < 20)
        1
      else if (pop < 50)
        2
      else
        4  
  
  def getArrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]] = {
    val arrivals = mutable.Map[Int, mutable.Map[Int,List[FFleet]]]()
    for {
      (planetId, ars) <- model.arrivals
      arrivalsOnPLanet = arrivals.getOrElseUpdate(planetId, mutable.Map())
      (turn, arList) <- ars
    } {
      arrivalsOnPLanet.put(turn, arList.map(flight => FFleet(flight.owner, flight.current.expected)))
    }
    arrivals
  }

  @tailrec
  private def calculateBattle(fleets: List[FFleet], fleetsInvolved: Int): Option[FFleet] = {
    fleets.size match {
      case 0 => None
      case 1 => Some(fleets.head)
      case _ => {
        val afterBattle = fleets.map {
          fleet =>
            val others = fleets.filter(_ != fleet)
            val afterRound = 
              (fleet.size - others.map(x => Math.ceil(x.size.toDouble / (fleetsInvolved - 1)).toInt).sum) max 0
            FFleet(fleet.owner, afterRound)
        }
        calculateBattle(afterBattle.filter(_.size > 0), fleetsInvolved)
      }
    }
  }
  
  //adds growth
  def resolveFPlanet(previous: FPlanet,
      arrivals: Option[List[FFleet]],
      departure: Option[FFleet]): FPlanet = {
    //TODO take into consideration departure
    
    val planetFleet = FFleet(previous.owner, previous.size + growth(previous.owner, previous.size))
    val fleets = arrivals match {
      case None => List(planetFleet)
      case Some(ars) => planetFleet :: ars
    }
    val grouped = fleets.groupBy(_.owner)
    val forces = for (force <- grouped)
      yield (FFleet(force._1, force._2.map(_.size).sum))
    if (forces.exists(_.owner != previous.owner)) {
      val winner = calculateBattle(forces.toList, forces.size) match {
        case None => None
        case w @ Some(FFleet(o, s)) =>
          if (previous.owner != o) {
            if (s > 1)
              Some(FFleet(o, s - 1))
            else
              None
          } else 
            w
      }
      winner match {
        case Some(w) => FPlanet(w.owner, w.size)
        case None => FPlanet(NeutralPlanet, 0)
      }
    } else {
      FPlanet(previous.owner, planetFleet.size)
    }
  }

  /**
   * Calculate balance for a planet. If balance is negative, the
   * returned value is number of ships needed to maintain the planet. If balance
   * is positive the returned value is number of ships which can be sent out.
   */
  def getBalance(arrivals: mutable.Map[Int, mutable.Map[Int, List[FFleet]]],
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]]): mutable.Map[Int, mutable.Map[Int, FPlanet]] = {
    val balance = mutable.Map[Int, mutable.Map[Int, FPlanet]]()
    for ((planetId, states) <- model.timeline) {

      val planetArrivals = arrivals.getOrElse(planetId, mutable.Map[Int, List[FFleet]]())
      val balances = mutable.Map[Int, FPlanet]()
      balance(planetId) = balances
      val turns = population(planetId)
      
      var neededForNextTurn = 1
      for (t <- MovesAhead to 0 by -1) {
        val turnArrivals = planetArrivals.get(turn + t + 1)

        val prev = turns(t-1)
            
        val cur = turns(t)

        val planetFleet = FFleet(prev.owner, prev.size + growth(prev.owner, prev.size))
        val fleets = turnArrivals match {
          case None => List(planetFleet)
          case Some(ars) => planetFleet :: ars
        }
        val grouped = fleets.groupBy(_.owner)
        
        val forces = (for (force <- grouped)
          yield (FFleet(force._1, force._2.map(_.size).sum))).toList

        val maxOpponent: Int =
          forces.filter(_.owner != prev.owner) match {
            case Nil => 0
            case l =>
              l.reduce((x, y) => if (x.size < y.size) x else y).size + 1  //+1 because we need to win
          }

        val defending = forces.find(_.owner == prev.owner).get.size
        
        val balance =
        if (prev.owner != cur.owner)
          //planet was lost, calculate negative balance
          (defending - maxOpponent - neededForNextTurn) min 0 //should be negative
        else
          //planet maintained, calculate positive balance
          (cur.size + growth(cur.owner, cur.size) - neededForNextTurn) max 0
          
        balances(t) = FPlanet(prev.owner, balance)
        
        neededForNextTurn =
          if (balance < 0)
            0
          else
            (maxOpponent - (defending - (prev.size + growth(prev.owner, prev.size))) + neededForNextTurn) max 1
      }
    }
    balance
  }
        
  def generateFuture(arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]]): mutable.Map[Int, mutable.Map[Int, FPlanet]] = {
    val population = mutable.Map[Int, mutable.Map[Int, FPlanet]]()
    for ((planetId, states) <- model.timeline) {
      val planetArrivals = arrivals.getOrElse(planetId, mutable.Map[Int,List[FFleet]]())
      val currentState = states(turn)
      val turns = mutable.Map[Int, FPlanet]()
      population(planetId) = turns
      
      val exp = currentState.current.expected
      val withoutGrowth =
        if (exp >= 54)
          exp - 4
        else if (exp >= 22)
          exp -2
        else
          exp -1
      turns(-1) = FPlanet(currentState.owner, withoutGrowth)
      for (t <- 0 to MovesAhead) {
        turns(t) = resolveFPlanet(turns(t - 1),
            planetArrivals.get(turn + t + 1), None)
      }
    }
    population
  }
  
  /*
   * -------------- past model -------------
   */
  
  def updateModel(universe: Universe, events: List[Event]) = {
    
    val departures = events.filter(_.getEventType() == EventType.LAUNCH)
    					.map(x => (x.getFromPlanet(), x)).toMap
    
    for ((planetId, states) <- model.timeline) {
      val lastTurnState = states(turn - 1)
      val currentUniversePlanet = universe.getPlanetMap().get(planetId)
      val arrivalsMap = model.arrivals.getOrElseUpdate(planetId, mutable.Map())
      
      
      //departures
      //first create flight
      val departure = departures.get(planetId).map {
        event =>
          //if this is our flight we know exact value
          val exactValue =
            if (event.getFleetOwner() == playerNumber)
              Some(Val(event.getSentShipCount()))
            else
              None
          val flight = new Flight(lastTurnState,
                                  event.getFleetSize(),
                                  Universe.getTimeToTravel(currentUniversePlanet,
                                  universe.getPlanetMap().get(event.getToPlanet())),
                                  lastTurnState.owner,
                                  event.getToPlanet(),
                                  exactValue)
          if (flight.owner == playerNumber)
            flight.set(Val(event.getSentShipCount()));
          val landingTurn = turn -1 + flight.duration
          val arrivalMap4Destination = model.arrivals.getOrElseUpdate(event.getToPlanet(), mutable.Map())
          val arrivals = flight :: arrivalMap4Destination.getOrElseUpdate(landingTurn, Nil)
          arrivalMap4Destination.update(landingTurn, arrivals)
          flight
      }

      //TODO testing
      StormTesters.accuracyTester.foreach {
        tester =>
            departure.foreach {
              d => {
                val e = departures.get(planetId).get
                tester.addMeasurmentForFlight(e.getFromPlanet(), e.getToPlanet(), d.current)
              }
            }
      }

      //possibility that planet was abandoned
      val possiblyAbandoned =
        lastTurnState.owner != NeutralPlanet &&
        possiblyInvalid(lastTurnState.current - departure.map(_.current).getOrElse(Zero)).intersects(Zero)

      //arrivals
      val arrivalsOption = arrivalsMap.get(turn)
      val arrivalsSum = arrivalsOption.map(_.map(_.current).reduce(_ + _))
      
      val playersOnPlanet = arrivalsOption match {
        case None => lastTurnState.owner :: Nil
        case Some(l) => (lastTurnState.owner :: l.map(_.owner)).distinct
      }
      
      //create current turn planet state
      val currentPlanetState = playersOnPlanet.size match {
        case 1 => {
          //friendly mode
          
          if (currentUniversePlanet.getOwner() == playerNumber) {
            //our planet - we know exact numbers
            val planet = PlanetState(planetId,
                        new Node(Val(currentUniversePlanet.getPopulation()),
                                 SizeRanges(currentUniversePlanet.getSize())),
                        turn,
                        playerNumber,
                        currentUniversePlanet.getSize() :: Nil)
            new Initial(planet, Val(currentUniversePlanet.getPopulation()))      
            new EndOfLine(lastTurnState)
            planet
          } else {
            val state = if (currentUniversePlanet.getOwner() != lastTurnState.owner) {
              //abandoned planet
              val planet = PlanetState(planetId,
                new Node(Zero,
                         SizeRanges(Size.SMALL)),
                         turn,
                         NeutralPlanet,
                         Size.SMALL :: Nil)
              new Initial(planet, Zero)
              new EndOfLine(lastTurnState)
              planet
            } else {
              lastTurnState.createNextTurnState(currentUniversePlanet.getSize(),
                currentUniversePlanet.getOwner(), departure.map(_.current),
                arrivalsSum,
                possiblyAbandoned)
            }

            if (possiblyAbandoned && (currentUniversePlanet.getOwner() != NeutralPlanet) &&
              arrivalsOption.isDefined)
              new TakingOverPlanet(state, arrivalsSum.get - ListVar(List(0, 1)), arrivalsOption)
            else
              for {
                arrivals <- arrivalsOption
                arrival <- arrivals
              } arrival.setTarget(state)

            StormTesters.accuracyTester.foreach {
              tester =>
                tester.addMeasurmentForPlanet(planetId, state.population.current, state, states)
            }

            state
          }
          
        }
        case _ => {
          //combat mode

            val landings = events.filter(x => x.getEventType() == EventType.LANDING &&
              x.getToPlanet() == planetId)

            val abandoned = !landings.exists(_.getFleetOwner() == lastTurnState.owner)

            if (playersOnPlanet.contains(playerNumber) &&
                //not abandoned by us
                (lastTurnState.owner != playerNumber || !abandoned)
                ) {
            //we know exact fleet sizes, so we can make use of this knowledge

            val arrivalsMap = arrivalsOption.get.groupBy(_.owner)
            
            landings.filter(_.getFleetOwner() != playerNumber).foreach {
              landing =>
                for {
                  arrivals <- arrivalsMap.get(landing.getFleetOwner())
                  arrival <- arrivals
                } {
                  if (arrivals.size == 1) {
                    val candidate =
                      if (arrivals.head.owner != lastTurnState.owner)
                        Val(landing.getSentShipCount())
                      else {
                        Val(landing.getSentShipCount()) -
                          (lastTurnState.current - departure.map(_.current).getOrElse(Zero))
                      }
                    if (candidate.isMorePreciseThan(arrival.current))
                      arrival.backPropagate(candidate, MaxPropagationDepth)
                  }
                  //TODO in future, same logic as with propagating with edges - isolate single edge
                  //and see if this enforces new constraint
                }
            }
          }
          
          if (currentUniversePlanet.getOwner() == playerNumber) {
            //we know exact planet's population
            val state = PlanetState(planetId,
                        new Node(Val(currentUniversePlanet.getPopulation()),
                                 SizeRanges(currentUniversePlanet.getSize())),
                        turn,
                        playerNumber,
                        currentUniversePlanet.getSize() :: Nil)
            new FromCombat(state, Val(currentUniversePlanet.getPopulation()), arrivalsOption.get, lastTurnState)
            new EndOfLine(lastTurnState)
            state

            StormTesters.accuracyTester.foreach {
              tester =>
                tester.addMeasurmentForPlanet(planetId, state.population.current, state, states)
            }

            state
            
          } else {
            //estimate
            val forcesList =
              //first element is planet's owner last turn's state
              //subtracted by possible departure 
              (lastTurnState.owner,
                departure match {
                  case None => lastTurnState.current
                  case Some(dep) => possiblyInvalid(lastTurnState.current - dep.current)
                }) ::
                (for (arrival <- arrivalsOption.get)
                  yield (arrival.owner, arrival.current))

            val grouped = forcesList.groupBy(_._1)

            val forces = for (singleForce <- grouped)
              yield (singleForce._1, singleForce._2.map(_._2).reduce(_ + _))

            val oppositeForcesPenalty = resolvePastBatlle(forces, possiblyAbandoned, currentUniversePlanet.getOwner())

            val abandonedPenalty =
              if (possiblyAbandoned)
                RangeVar(0, 1)
              else
                Zero

            val victorForces =
              if (lastTurnState.owner == currentUniversePlanet.getOwner()) {
                possiblyInvalid(forces(currentUniversePlanet.getOwner()) - (oppositeForcesPenalty + abandonedPenalty))
              } else {
                if (currentUniversePlanet.getOwner() == NeutralPlanet) {
                  if (playersOnPlanet.contains(NeutralPlanet))
                    (forces(NeutralPlanet) - oppositeForcesPenalty) and NonNegative
                  else
                    Zero
                } else
                  //RangeVar(0, 1), because if planet was not occupied
                  (forces(currentUniversePlanet.getOwner()) - (oppositeForcesPenalty + Val(1))) and
                    Positive
              }

            val newPopulationIncludingGrowth = currentUniversePlanet.getOwner() match {
              case NeutralPlanet => victorForces
              case _ => victorForces + growthValForSize(sizesForVar(victorForces), victorForces, None, None, false)
            }

            val state = PlanetState(planetId,
              new Node(newPopulationIncludingGrowth, SizeRanges(currentUniversePlanet.getSize())),
              turn,
              currentUniversePlanet.getOwner(),
              currentUniversePlanet.getSize() :: Nil)

            val growth = state.owner match {
              case NeutralPlanet => None
              case _ => Some(new Growth(state, growthValForSize(sizesForVar(victorForces),
                victorForces, None, None, false)))
            }

            new FromCombat(state, victorForces, arrivalsOption.get, lastTurnState)

            //attach outgoing edge from lastTurn, so that flight rooted at that state does not get rebalanced
            new EndOfLine(lastTurnState)

            StormTesters.accuracyTester.foreach {
              tester =>
                tester.addMeasurmentForPlanet(planetId, state.population.current, state, states)
            }

            state
          }
        }
      }
      
      //set new value calculated from incoming edges
      currentPlanetState.set(currentPlanetState.population.calculate(currentPlanetState.population.incoming))
      states(turn) = currentPlanetState
      
      lastTurnState.population.applyMask
      currentPlanetState.population.applyMask

      lastTurnState.population.propagateBothWays(MaxPropagationDepth)
      currentPlanetState.population.propagateBothWays(MaxPropagationDepth)
     
    }
    
    //TODO testing
    StormTesters.accuracyTester.foreach {
      tester =>
        for ((planetId, states) <- model.timeline) {
          val cur = states(turn)
          tester.addMeasurmentForPlanet(planetId, cur.population.current, cur, states)
        }
    }
        
  }
  
  def initializeModel(universe: Universe) = {
    numberOfPlanets = universe.getPlanets().size()
    
    val distances =
      for {
        x <- 0 to numberOfPlanets - 1
        y <- 0 to numberOfPlanets - 1
      } yield ((x, y),
        if (x == y)
          0
        else
          Universe.getTimeToTravel(universe.getPlanetMap().get(x), universe.getPlanetMap().get(y))
          )
          
    planetDistances = distances.toMap
    
    val allDistances =
      for {
        x <- 0 to numberOfPlanets - 1
        y <- 0 to numberOfPlanets - 1 if y != x
      } yield (x, y, planetDistances(x, y))
      
    val groupedAllDistances =
      allDistances.groupBy(_._1)
      
    val sortedAllDistances =
      groupedAllDistances.map {
        case (k, v) => (k, v.sortBy(_._3))
      }
      
    planetsByDistance = sortedAllDistances.map {
      case (k, v) => (k, v.map(x => (x._2, x._3)).toList)
    }
      
    for ((id, planet) <- universe.getPlanetMap()){
      model.timeline(id.toInt) =
        mutable.Map(0 -> PlanetState.initialPlanetState(planet.getId(), planet.getOwner(), planet.getSize()))
    }
  }
  
}