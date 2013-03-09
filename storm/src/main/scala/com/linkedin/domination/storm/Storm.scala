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
  
  val output= new PrintWriter("/Users/jodzga/git/storm/storm/visualizer/replay/addon.json")
  output.write("{ \"data\": [\n")
  
  def initialize(playerNbr: java.lang.Integer) = {
    playerNumber = playerNbr
    delegate.initialize(playerNbr)
  }
  
  // ** constants **
  val MovesAhead = 48
  val MaxAttackTimeSpan = 20
  
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
  
  /**
   * everything in the model is defined in the model's turns
   * [planet][turn][...]
   */
  case class Model(timeline: mutable.Map[Int, mutable.Map[Int, PlanetState]],
                   arrivals: mutable.Map[Int, mutable.Map[Int, List[Flight]]],
                   scheduledDepartures: mutable.Map[Int, mutable.Map[Int, FFlight]]) {
    
    var turn = 0

    @inline
    def relativeToModelTurn(t: Int) = turn + t + 1
    
    @inline
    def modelToRelative(trn: Int) = trn - turn - 1
  }
  
  /**
   * score = accumulated growth of me - accumulated growth of opponents (based on futures)
   */
  
  
  
  val model: Model = Model(mutable.Map(), mutable.Map(), mutable.Map()) //initially empty model
  
  var timing: Long = System.currentTimeMillis()
  
  def makeMove(universe: Universe, events: java.util.List[Event]): java.util.List[Move] = {  
      
    //update model past
    if (model.timeline.isEmpty)
      initializeModel(universe)
    else
      updateModel(universe, events.toList);
      
    //calculate future
    val (arrivals, departures) = getArrivalsAndDepartures
    
    val (futureBase, futureArs, futureDeps) = generateFuture(arrivals, departures)
    
    val (balance, targets) = getBalanceAndTargets(futureArs, futureDeps, futureBase)
    writeOutEstimates(output, balance)
    
    //calculate moves
    val partialMoves = getPartialMoves(targets, balance, futureBase)
    
    val (scheduledMoves, newMoves) = partialMoves.partition(_.scheduled)
    
    val baselineAccGrowth =
      (for {
        (planetId, turns) <- futureBase
        (turn, state) <- turns
      } yield {
        state match {
          case FPlanet(player, size) if (player == playerNumber) => growth(size)
          case _ => 0
        }
      }).sum

    //go parallel 
    val scoredMoves = newMoves./*par.*/map {
      move => (score(move, futureBase, futureArs, futureDeps, baselineAccGrowth), move)
    }
    
    val sortedByScore = scoredMoves.filter(_._1 > 0).toList.sortBy(_._1).reverse.map(_._2)
    
    //combine moves
    val combined = combineMoves(scheduledMoves.toList ::: sortedByScore)
    
    //for this turn only schedule moves which start in this turn
    val filtered = filterMoves(combined)
    
    rememberScheduledMoves(filtered)
    
    //update turn
    model.turn += 1
    
//    val newTime = System.currentTimeMillis()
//    println("turn: " + turn + ", moves: " + sortedByScore.size + ", time: " + (newTime - timing))
//    timing = newTime
      
    //return moves
    toGameMoves(filtered)
  }
  
  def rememberScheduledMoves(combined: List[TargetedMove]) = {
    for {
      move <- combined
      f <- move.flights
    } {
      if (f.turnDepart > 0) {
        val scheduledTurns = model.scheduledDepartures.getOrElseUpdate(f.from, mutable.Map[Int, FFlight]())
        scheduledTurns(model.relativeToModelTurn(f.turnDepart)) =
          f.copy(turnDepart = 0, turnArrive = f.turnArrive - f.turnDepart)
      }
    }
  }
  
  def toGameMoves(combined: List[TargetedMove]): List[Move] = {
    for {
      move <- combined
      f <- move.flights if (f.turnDepart == 0)
    } yield (new Move(f.from, f.to, f.fleetType))
  }

  def filterMoves(moves: List[TargetedMove]): List[TargetedMove] = {
    for (move <- moves if move.flights.exists(f => f.turnDepart == 0))
      yield move
  }
  
  def combineMoves(moves: List[TargetedMove]): List[TargetedMove] = {
    var usedPlanets = mutable.Set[Int]()
    for {
      move <- moves
      if (!move.flights.exists(f => usedPlanets(f.from)))
    }
      yield {
        usedPlanets ++= move.flights.map(_.from)
        move
      }
  }
  
  def writeOutEstimates(output: PrintWriter, sts: mutable.Map[Int, mutable.Map[Int,FPlanet]]) = {
    output.write("  {\n")

    for ((planetId, states) <- sts) {
      val cur = states(0)
      output.write("    \"" + planetId + "\": \"" + cur.size +
        "\",\n")
    }
    output.write("  },\n")
    output.flush()
  }
  
  /*
   * -------------- future model -------------
   */
  
  case class FFleet(owner: Int, size: Int)
  
  case class FPlanet(owner: Int, size: Int)
  
  case class FFlight(from: Int, to: Int, size: Int, fleetType: FleetType, turnDepart: Int, turnArrive: Int) {
    override def toString = "from: " + from + ", to: " + to + ", size: " + size + ", depart: " + turnDepart + ", arrive: " + turnArrive 
  }
  
  case class TargetedMove(owner: Int, target: Int, flights: List[FFlight], scheduled: Boolean) {
    val timeSpan = flights match {
      case Nil => 0
      case _ => flights.map(x => x.turnArrive - x.turnDepart).max
    }
    
    val shipsSpent = flights.map(_.size).sum
  }
  
  /**
   * Score is difference between accumulated growth after move
   * and accumulated growth before, divided by ships spent
   */
  def score(move: TargetedMove,
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]],
      arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]],
      departures: mutable.Map[Int, mutable.Map[Int, FFleet]],
      baselineAccGrowth: Int): Double = {
    (calculateAccGrowth(move, population, arrivals, departures, baselineAccGrowth) - baselineAccGrowth).toDouble / move.shipsSpent
  }
  
  case class FPlanetUpdate(prevState: FPlanet, departure: Option[FFleet], arrivals: Option[List[FFleet]])
  
  /**
   * for simplicity right now departures and arrivals are not recalculated
   */
  def calculateAccGrowth(move: TargetedMove, 
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]], 
      arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]], 
      departures: mutable.Map[Int, mutable.Map[Int, FFleet]],
      baselineAccGrowth: Int): Int = {
    
    val turnsToRecalculate =
      ArrayBuffer.fill[mutable.Map[Int,FPlanetUpdate]](MovesAhead + 1)(mutable.Map[Int,FPlanetUpdate]())
    
    for (flight <- move.flights) {
      val previousStateDep = population(flight.from)(flight.turnDepart - 1)
      val size = previousStateDep.size + growth(playerNumber, previousStateDep.size)
      val fleet = FFleet(playerNumber, flight.fleetType match {
        case FleetType.SCOUTING => (size * 0.25).toInt
        case FleetType.RAIDING => (size * 0.5).toInt
        case FleetType.ASSAULT => (size * 0.75).toInt
        case FleetType.HORDE => size
      })
      //add departure
      val existingDep = turnsToRecalculate(flight.turnDepart)
      val updatedDep = existingDep.get(flight.from) match {
        case None =>
          FPlanetUpdate(previousStateDep, Some(fleet), None)
        case Some(FPlanetUpdate(prevSize, None, arrOpt)) =>
          FPlanetUpdate(prevSize, Some(fleet), arrOpt)
      } 
      existingDep(flight.from) = updatedDep
      //add arrivals
      val existingArr = turnsToRecalculate(flight.turnArrive)
      val updatedArr = existingArr.get(flight.to) match {
        case None =>
          FPlanetUpdate(population(flight.to)(flight.turnArrive - 1), None, Some(List(fleet)))
        case Some(FPlanetUpdate(prevSize, depOpt, arrOpt)) =>
          val updatedArrList = arrOpt match {
            case None => List(fleet)
            case Some(l) => fleet :: l
          }
          FPlanetUpdate(prevSize, depOpt, Some(updatedArrList))
      }
      existingArr(flight.to) = updatedArr
    }
    
    var currentScore = baselineAccGrowth
    
    for {
      t <- 0 to MovesAhead
      mp = turnsToRecalculate(t)
      (planetId, FPlanetUpdate(prevState, depOpt, arrOpt)) <- mp
    } {
      val mergedArrivals = arrivals.get(planetId).flatMap(_.get(t)) match {
        case None => arrOpt
        case x @ Some(l) => arrOpt match {
          case None => x
          case Some(k) => Some(l ::: k)
        }
      }
      
      val mergedDepartures = departures.get(planetId).flatMap(_.get(t)) match {
        case None => depOpt
        case x => x
      }
      
      val updatedState = resolveFPlanet(prevState,
            mergedArrivals,
            mergedDepartures)
      
      val oldState = population(planetId)(t)
      
      if (oldState.owner == playerNumber)
        currentScore -= growth(oldState.size)
        
      if (updatedState.owner == playerNumber)
        currentScore += growth(updatedState.size)
      
      if (updatedState != oldState && t < MovesAhead)
        turnsToRecalculate(t + 1).put(planetId, FPlanetUpdate(updatedState, None, None))
      
    }
    
    currentScore
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
  
  /**
   * TODO for defensive actions consider looking for moves in earlier turns with
   * smaller size
   */
  def getPartialMoves(targets: mutable.Map[Int, mutable.Map[Int, FPlanet]],
      balances: mutable.Map[Int, mutable.Map[Int, FPlanet]],
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]]): List[TargetedMove] = {
    
    val scheduledMoves =
      for {
        (p, scheduledTurns) <- model.scheduledDepartures
        dep <- scheduledTurns.get(model.relativeToModelTurn(0))
        if (balances(p)(0).owner == playerNumber)
      } yield {
        TargetedMove(balances(dep.to)(dep.turnArrive).owner,
            dep.to, List(dep), true)
      }
      
    val planetUsedInScheduledMoves = scheduledMoves.flatMap(_.flights.map(_.from)).toSet
      
    def partialMovesForPlanet(t: Int, planet: Int, turnTargets: mutable.Map[Int,FPlanet]): Option[TargetedMove] = {

	  @tailrec
	  def findTragetedMove(tgt: FPlanet, planets: List[(Int, Int)], soFar: List[FFlight]): List[FFlight] = {
	    if (tgt.size <= 0)
	      soFar
	    else if (planets.isEmpty)
	      Nil
	    else {
	      val (p, dist) = planets.head
	      val pt = t - (dist - 1)
	      if (pt >= 0) {
	        val b = balances(p)(pt)
	        if (b.owner == playerNumber && b.size > 0) {
	          
	          val possibleFleets =
	            for {
	              (fs, d) <- FleetSizes if (fs != FleetType.HORDE)  //we don't want to abandon planet
	              ppop = population(p)(pt - 1)
	              if (ppop.owner == playerNumber)  //this should be true
	              pop = ((ppop.size + growth(ppop.owner, ppop.size)) * d).toInt //to send
	              if (pop < b.size)  //must be less than a balance
//	              if (Size.getSizeForNumber(ppop.size + growth(ppop.owner, ppop.size) - pop) == 
//	                  Size.getSizeForNumber(population(p)(pt).size))  //must not affect planet size
	            }
                  yield (fs, pop)
                  
              possibleFleets match {
                case Nil => findTragetedMove(tgt, planets.tail, soFar)
                case x => {
                  val satisfying = x.filter(_._2 >= tgt.size)
                  //best fit is smallest value which fulfills target
                  //or largest possible
                  val bestFit = satisfying match {
                    case Nil => x.maxBy(_._2)
                    case y => y.minBy(_._2)
                  }
                  findTragetedMove(FPlanet(tgt.owner, tgt.size - bestFit._2),
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
        //support stealing - if previous one was taken from neutral, then it is
        //better to take if from non-neutral in later turn
        //otherwise, the sooner the better 
        //TODO need cooperation with target selectors
        partialMovesForPlanet(t - 1, planet, turnTargets) match {
          case x @ Some(move) => {
            if (move.owner == NeutralPlanet) {
              turnTargets.get(t) match {
                case Some(tgt) if tgt.owner != NeutralPlanet =>
                  findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => planetUsedInScheduledMoves(x._1)), Nil) match {
                    case Nil => x
                    case y => Some(TargetedMove(tgt.owner, planet, y, false))
                  }
                case _ => x 
              }
            } else
              x
          }
          case None => {
            turnTargets.get(t) match {
              case None => None
              case Some(tgt) =>
                findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => planetUsedInScheduledMoves(x._1)), Nil) match {
                  case Nil => None
                  case x => Some(TargetedMove(tgt.owner, planet, x, false))
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
    
    val filtered = partialMoves.foldLeft(List[TargetedMove]()) {
      case (l, Some(e)) =>  {
        //filter out too long moves
        if (!(e.timeSpan > MaxAttackTimeSpan))
          e :: l
        else
          l
      }
      case (l, None) => l
    }
    
    filtered ++ scheduledMoves
  }

  def growth(size: Int): Int =
    if (size < 20)
      1
    else if (size < 50)
      2
    else
      4  
  
  def growth(owner: Int, pop: Int): Int =
    if (owner == NeutralPlanet)
      0
    else
      growth(pop)
      
  /**
   * result is in model's turn - TODO change it
   */
  def getArrivalsAndDepartures = {
    val arrivals = mutable.Map[Int, mutable.Map[Int,List[FFleet]]]()
    val departures = mutable.Map[Int, mutable.Map[Int,FFlight]]()
    for {
      (planetId, ars) <- model.arrivals
      arrivalsOnPLanet = arrivals.getOrElseUpdate(planetId, mutable.Map())
      (turn, arList) <- ars
    } {
      arrivalsOnPLanet.put(model.modelToRelative(turn), arList.map(flight => FFleet(flight.owner, flight.current.expected)))
    }
    
    for {
      (planetId, flights) <- model.scheduledDepartures
      departuresOnPLanet = departures.getOrElseUpdate(planetId, mutable.Map())
      (turn, f) <- flights
    } {
      departuresOnPLanet.put(model.modelToRelative(turn), f)
      val arrivalsOnPLanet = arrivals.getOrElseUpdate(f.to, mutable.Map())
      val existing = arrivalsOnPLanet.get(model.modelToRelative(turn + f.turnArrive))
      val arrival = FFleet(playerNumber, f.size)  //TODO this can be potentially improved
      val updated = existing match {
        case None => List(arrival)
        case Some(l) => arrival :: l
      }
      arrivalsOnPLanet(model.modelToRelative(turn + f.turnArrive)) = updated
    }
    
    (arrivals, departures)
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

    val planetFleet = FFleet(previous.owner,
      previous.size + growth(previous.owner, previous.size) - (departure match {
        case None => 0
        case Some(f) => f.size
      }))
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
      FPlanet(previous.owner, forces.find(_.owner == previous.owner).get.size)
    }
  }

  /**
   * TODO test!
   * Calculate balance for a planet. If balance is negative, the
   * returned value is number of ships needed to maintain the planet. If balance
   * is positive the returned value is number of ships which can be sent out.
   */
  /**
   * TODO don't mark neutral as a target if there is an enemy close by (stealing) 
   * Returns possible targets (how many ships need to be sent there)
   * for attack and defense actions.
   * [planet][turn]
   */
  def getBalanceAndTargets(arrivals: mutable.Map[Int, mutable.Map[Int, List[FFleet]]], 
      departures: mutable.Map[Int, mutable.Map[Int, FFleet]], 
      population:  mutable.Map[Int, mutable.Map[Int, FPlanet]]):
      (mutable.Map[Int, mutable.Map[Int, FPlanet]], mutable.Map[Int, mutable.Map[Int, FPlanet]]) = {
    
    val balance = mutable.Map[Int, mutable.Map[Int, FPlanet]]()
    val target = mutable.Map[Int, mutable.Map[Int, FPlanet]]()
    
    for ((planetId, _) <- model.timeline) {

      val planetArrivals = arrivals.getOrElse(planetId, mutable.Map[Int, List[FFleet]]())
      val planetDepartures = departures.getOrElse(planetId, mutable.Map[Int, FFleet]())
      val balances = mutable.Map[Int, FPlanet]()
      balance(planetId) = balances
      
      val targetTurns = mutable.Map[Int, FPlanet]()
      target(planetId) = targetTurns
      
      val turns = population(planetId)
      var planetOwnedInFuture = false
      var battleInTheFuture = false
      var departureInTheFuture = false
      
      for (t <- MovesAhead to 0 by -1) {
        val turnArrivals = planetArrivals.get(t)
        val turnDeparture = planetDepartures.get(t)

        val prev = turns(t-1)
        val cur = turns(t)
        
        if (cur.owner == playerNumber)
          planetOwnedInFuture = true
        
        val departureSize = turnDeparture match {
          case None => 0
          case Some(f) =>
            f.size
        }

        val planetFleet = FFleet(prev.owner, prev.size + growth(prev.owner, prev.size) - departureSize)
        val fleets = turnArrivals match {
          case None => List(planetFleet)
          case Some(ars) => planetFleet :: ars
        }
        val grouped = fleets.groupBy(_.owner)
        
        val forces = (for (force <- grouped)
          yield (FFleet(force._1, force._2.map(_.size).sum))).toList
          
        if (forces.size > 1)
          battleInTheFuture = true

        val maxOpponent: Int =
          forces.filter(_.owner != prev.owner) match {
            case Nil => 0
            case l =>
              l.reduce((x, y) => if (x.size < y.size) x else y).size
          }

        val defending = forces.find(_.owner == prev.owner).get.size
        
        /**
         * TODO reflect future needs in the balance
         * easiest approach: if there is a battle in this
         * planet in the future and I own it, then balance is 0
         */
        val balance =
          if (prev.owner == NeutralPlanet)
            0
          else if (prev.owner == playerNumber && (battleInTheFuture || departureInTheFuture))
            0
          else if (prev.owner == cur.owner)
            cur.size - 1
          else
            defending - maxOpponent - 1
        
        if (departureSize > 0)
          departureInTheFuture = true
        
        balances(t) = FPlanet(prev.owner, balance)
        
        if (prev.owner == playerNumber && balance < 0) {
          //defense
          targetTurns(t) = FPlanet(prev.owner, -balance)
        } else if (prev.owner != playerNumber && !planetOwnedInFuture) {
          //attack
          val forcesToTakePlanet = defending + 2 -  //+2: 1 to take planet, 1 to keep it
            fleets.find(_.owner == playerNumber).map(_.size).getOrElse(0)
            
          targetTurns(t) = FPlanet(prev.owner, forcesToTakePlanet)
        }
        
      }
    }
    (balance, target)
  }
  
  
  /**
   * This method adds adequate values to departures and arrival depending
   * on the scheduled flights
   * 
   * returns (futureBase, departures, arrivals)
   */
  def generateFuture(arrivals: mutable.Map[Int, mutable.Map[Int,List[FFleet]]],
      departures: mutable.Map[Int, mutable.Map[Int,FFlight]]) = {
    val population = mutable.Map[Int, mutable.Map[Int, FPlanet]]()
    val deps = mutable.Map[Int, mutable.Map[Int, FFleet]]()
    
    for ((planetId, states) <- model.timeline) {
      val currentState = states(model.turn)
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
    }
    
    
      for {
        t <- 0 to MovesAhead
        (planetId, _) <- model.timeline
      } {
        val turns = population(planetId)
        //update departures and arrivals
        val previousState = turns(t - 1)
        departures.get(planetId).foreach(_.get(t).foreach {
          flight =>
            if (previousState.owner == playerNumber) {
              val size = previousState.size + growth(playerNumber, previousState.size)
              val planetDeps = deps.getOrElseUpdate(planetId, mutable.Map[Int, FFleet]())
              val fleet = FFleet(playerNumber, flight.fleetType match {
                case FleetType.SCOUTING => (size * 0.25).toInt
                case FleetType.RAIDING => (size * 0.5).toInt
                case FleetType.ASSAULT => (size * 0.75).toInt
                case FleetType.HORDE => size
              }) 
              planetDeps(t) = fleet
              val planetArs = arrivals.getOrElseUpdate(planetId, mutable.Map[Int,List[FFleet]]())
              planetArs(t + flight.turnArrive) = planetArs.get(t + flight.turnArrive) match {
                case None => List(fleet)
                case Some(l) => fleet :: l
              }
            }
        })
        turns(t) = resolveFPlanet(previousState,
            arrivals.get(planetId).flatMap(_.get(t)),
            deps.get(planetId).flatMap(_.get(t)))
      }
    (population, arrivals, deps)
  }
  
  /*
   * -------------- past model -------------
   */
  
  def updateModel(universe: Universe, events: List[Event]) = {
    
    val departures = events.filter(_.getEventType() == EventType.LAUNCH)
    					.map(x => (x.getFromPlanet(), x)).toMap
    
    for ((planetId, states) <- model.timeline) {
      val lastTurnState = states(model.turn - 1)
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
          val landingTurn = model.turn -1 + flight.duration
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
      val arrivalsOption = arrivalsMap.get(model.turn)
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
                        model.turn,
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
                         model.turn,
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
                        model.turn,
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
              model.turn,
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
      states(model.turn) = currentPlanetState
      
      lastTurnState.population.applyMask
      currentPlanetState.population.applyMask

      lastTurnState.population.propagateBothWays(MaxPropagationDepth)
      currentPlanetState.population.propagateBothWays(MaxPropagationDepth)
     
    }
    
    //TODO testing
    StormTesters.accuracyTester.foreach {
      tester =>
        for ((planetId, states) <- model.timeline) {
          val cur = states(model.turn)
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