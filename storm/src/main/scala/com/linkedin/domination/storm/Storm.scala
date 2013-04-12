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
import scala.io._
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import com.linkedin.domination.api.Move.FleetType
import scala.collection.mutable.ListBuffer
import scala.util.Random


class Storm extends Player {
  
  type Owner = Int
  type PlanetId = Int
  type Turn = Int
  type Distance = Int
  type Population = Int
  type Player = Int

  val getPlayerName = "Storm"
    
  var playerNumber = 0
  
//  val output= new PrintWriter("/Users/jodzga/git/storm/storm/visualizer/replay/addon.json")
//  output.write("{ \"data\": [\n")
  
  def initialize(playerNbr: java.lang.Integer) = {
    playerNumber = playerNbr
  }
  
  // ** constants **
  val MovesAhead = 28
  val MaxAttackTimeSpan = 16
  
  val DefaultValueIfVeryLowConfidence = 67
  val LowConfidenceRange = 100
  
  var numberOfPlanets = 0;
  var planetDistances: Map[(PlanetId, PlanetId), Distance] = null
  
  var planetsByDistance: Map[PlanetId, List[(PlanetId, Distance)]] = null
  
  var planetsInStregthRadius: Map[PlanetId, List[(PlanetId, Distance)]] = null
  
  val MaxCachedPopForStream = 75
  //pre-calculated list of semi-optimal stream
  //max population is 75 
  val optimalStream: Map[(Distance, Population, Turn), IndexedSeq[Population]] = {
    val condidates = List((2, 1), (28, 20), (67, 50))
    val simulation =
      for {
        distance: Distance <- 1 to 65
        population: Population <- 0 to MaxCachedPopForStream
        t: Turn <- 0 to MovesAhead
      } yield {
        
        @tailrec
        def accumulate(trn: Turn, current: Population, min: Population, max: Population, 
            deps: ListBuffer[Population]): Unit = {
          if (trn <= MovesAhead) {
            if ((trn + (distance - 1) <= MovesAhead) && current > min) {
              val possibleFleets =
                for {
                  (fs, d) <- FleetSizes if (fs != FleetType.HORDE) //we don't want to abandon planet
                  pop = (current * d).toInt
                  if (current - pop >= min)
                } yield pop

              possibleFleets match {
                case Nil => {
                  accumulate(trn + 1, current + growth(1, current), min, max, deps += 0)
                }
                case fleets =>
                  val fmax = fleets.max
                  accumulate(trn + 1, current - fmax + growth(1, current - fmax), min, max, deps += fmax)
              }
            } else
              accumulate(trn + 1, current + growth(1, current), min, max, deps += 0)
          }
        }
        
        val simulations =
          for (candidate <- condidates)
            yield {
              val mp: ListBuffer[Population] = ListBuffer()
              accumulate(t, population, candidate._2, candidate._1, mp)
              (distance, population, t, mp)
            }
        
        val scored = simulations.map {
          x =>
            val score = x._4.sum
            (score, x)
        }
        
        val sorted = scored.sortBy(_._1).reverse
        
        (distance, population, t) -> sorted.head._2._4
      }
      
    simulation.map(x => (x._1, x._2.toIndexedSeq)).toMap
  }

  def accOptimalStreamUnbound(distance: Distance, population: Population, t: Turn): IndexedSeq[Population] = {
    for (k <- MovesAhead to t by -1)
      yield optimalStreamUnbound(distance, population, k).sum
  }
  
  def optimalStreamUnbound(distance: Distance, population: Population, t: Turn): IndexedSeq[Population] = {

    @tailrec
    def accumulate(trn: Turn, current: Population, deps: ListBuffer[Population]): Unit = {
      if (trn <= MovesAhead) {
        if (trn + (distance - 1) <= MovesAhead) {
          val possibleFleets: List[Population] =
            for {
              (fs, d) <- FleetSizes if (fs != FleetType.HORDE) //we don't want to abandon planet
              pop = (current * d).toInt
              if (current - pop >= 50)
            } yield pop

          possibleFleets match {
            case Nil => {
              accumulate(trn + 1, current + growth(1, current), deps += 0)
            }
            case fleets =>
              val fmax = fleets.max
              if (current - fmax + growth(1, current - fmax) > MaxCachedPopForStream)
                accumulate(trn + 1, current - fmax + growth(1, current - fmax), deps += fmax)
              else
                if (trn < MovesAhead)
                  (deps += fmax) ++= optimalStream(distance, current - fmax + growth(1, current - fmax), trn + 1)
          }
        } else {
          accumulate(trn + 1, current + growth(1, current), deps += 0)
        }
      }
    }
     
    if (population <= MaxCachedPopForStream)
      optimalStream(distance, population, t)
    else {
      val mp: ListBuffer[Population] = ListBuffer()
      accumulate(t, population, mp)
      mp.toIndexedSeq
    }
    
  }
  
  /**
   * everything in the model is defined in the model's turns
   */
  case class Model(timeline: mutable.Map[PlanetId, mutable.Map[Turn, PlanetState]],
                   arrivals: mutable.Map[PlanetId, mutable.Map[Turn, List[Flight]]],
                   scheduledDepartures: mutable.Map[PlanetId, mutable.Map[Turn, FFlight]]) {
    
    var turn = 0

    @inline
    def relativeToModelTurn(t: Int) = turn + t + 1
    
    @inline
    def modelToRelative(trn: Int) = trn - turn - 1
  }
  
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

    //uses original arrivals
    val (streams, strengths, states, statesUnderAttack, baselineAccGrowth) = calculateStrengthsBaseline(arrivals, departures)
//    writeOutStrengths(output, strengths)
   
    //note: modifies arrivals
    val (futureBase, futureArs, futureDeps) =
      generateFuture(arrivalsCopy(arrivals), departures)
//    writeOutFrontBack(output, futureBase)
    
    val (balance, targets) = getBalanceAndTargets(futureArs, futureDeps, futureBase)
//    writeOutBalance(output, balance)
    
    //calculate moves
    val partialMoves = getPartialMoves(targets, balance, futureBase)
    
    val (scheduledMoves, newMoves) = partialMoves.partition(_.scheduled)
    
    //go parallel
    val scoredMoves = newMoves.par.map {
      move => (score(move, streams, strengths, states, statesUnderAttack, arrivals, departures, baselineAccGrowth), move)
    }
    
    val sortedByScore = scoredMoves.filter(_._1 > 0).toList.sortBy(_._1).reverse.map(_._2)
    
    //combine moves
    val combined = combineMoves(scheduledMoves.toList ::: sortedByScore)
    
    //for this turn only schedule moves which start in this turn
    val filtered = filterMoves(combined)
    
    rememberScheduledMoves(filtered)
    
    //move forces from back to fronts and 
    val redistribution = redistributePlanets(futureBase, strengths, filtered)
    
    //update turn
    model.turn += 1
    
//    val newTime = System.currentTimeMillis()
//    println("turn: " + model.turn + ", moves: " + sortedByScore.size + 
//        ", filtered: " + filtered.size + ", time: " + (newTime - timing))
//    timing = newTime
      
//    writeOutTargets(output, filtered, redistribution)
    
    //return moves
    toGameMoves(filtered ::: redistribution)
  }

  def addArrivals(a: Option[List[FFleet]], b: Option[List[FFleet]]) = {
    a match {
      case None => b
      case Some(la) => b match {
        case (None) => a
        case Some(lb) => Some(la ::: lb)
      }
    }
  }
  
  def calculateStrengthsBaseline(arrivals: mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]],
      departures: mutable.Map[PlanetId, mutable.Map[Turn,FFlight]]) = {
    /**
     * streams:
     * [turn][from][to]->(owner, stream, idx)
     * idx is index in stream at turn t
     * 
     * strengths:
     * [planet][turn]->(player->population) 
     */ 
    val streams = Array.ofDim[(Player, IndexedSeq[Population], Int)](MovesAhead + 1, numberOfPlanets, numberOfPlanets)
    val strengths = Array.ofDim[Map[Player, Population]](numberOfPlanets, MovesAhead + 1)
    val states = Array.ofDim[FPlanet](numberOfPlanets, MovesAhead + 1)
    val statesUnderAttack = Array.ofDim[FPlanet](numberOfPlanets, MovesAhead + 1)
    
    val InitialStrength = Map(1 -> 0, 2 -> 0, 3 -> 0)
    
    var baselineAccGrowth = 0

    for {
      planet: PlanetId <- 0 until numberOfPlanets
      t <- 0 to MovesAhead
    } strengths(planet)(t) = InitialStrength
      
    val arrs = Array.fill(numberOfPlanets)(mutable.Map[Turn, List[FFleet]]())
      
    var previousStates = Array.ofDim[FPlanet](numberOfPlanets)
    
    for ((planetId, states) <- model.timeline) {
      val currentState = states(model.turn)
      val exp = expectedOrDefault(currentState.current)
      val withoutGrowth =
        if (currentState.owner != NeutralPlanet) {
          if (exp >= 54)
            exp - 4
          else if (exp >= 22)
            exp - 2
          else
            exp - 1
        } else
          exp
      previousStates(planetId) = FPlanet(currentState.owner, withoutGrowth)
    }

    for {
      t: Turn <- 0 to MovesAhead
      from: PlanetId <- 0 until numberOfPlanets
    } {
      //calculate state for the planet, take into consideration arrivals and departures
      val previousState = previousStates(from)

      val dep =
        if (previousState.owner == playerNumber)

          departures.get(from).flatMap(_.get(t).map {
            flight =>

              val size = previousState.size + growth(playerNumber, previousState.size)

              val fleet = FFleet(playerNumber, flight.fleetType match {
                case FleetType.SCOUTING => (size * 0.25).toInt
                case FleetType.RAIDING => (size * 0.5).toInt
                case FleetType.ASSAULT => (size * 0.75).toInt
                case FleetType.HORDE => size
              })

              arrs(flight.to)(t + flight.turnArrive) = arrs(flight.to).get(t + flight.turnArrive) match {
                case None => List(fleet)
                case Some(l) => fleet :: l
              }
              fleet
          })
        else
          None

      val arrivalsFromScheduledMoves = arrs(from).get(t)
      val arrivalsFromGame = arrivals.get(from).flatMap(_.get(t))
      
      val arrivalsFromStrength =
        (for ((planet, size) <- strengths(from)(t) if (size > 0))
          yield FFleet(planet, size)).toList match {
          case Nil => None
          case x => Some(x)
        }

      val currentState =
        resolveFPlanet(previousState,
          addArrivals(arrivalsFromScheduledMoves, arrivalsFromGame),
          dep)
          
      val currentStateUnderAttack =
        resolveFPlanet(previousState,
          addArrivals(arrivalsFromScheduledMoves, addArrivals(arrivalsFromGame, arrivalsFromStrength)),
          dep)
          
      if (currentState.owner == playerNumber && currentStateUnderAttack.owner == playerNumber)
        baselineAccGrowth += growth(currentState.size)
        
      if (currentState.owner != playerNumber && currentStateUnderAttack.owner != playerNumber &&
          currentState.owner != NeutralPlanet && currentStateUnderAttack.owner != NeutralPlanet)
        baselineAccGrowth -= growth(currentState.size)
          
      for (to: PlanetId <- 0 until numberOfPlanets if (to != from)) {

        //if owner has changed or turn 0, then need new plan, otherwise just continue plan from previous turn
        if (t == 0) {
          val size = previousState.size + growth(previousState.owner, previousState.size)
          streams(t)(from)(to) = (previousState.owner,
            accOptimalStreamUnbound(planetDistances(from, to), size, t),
            0)
        }

        //update stream for t+1 if t < MovesAhead
        if (t < MovesAhead) {
          if (previousState.owner != currentState.owner)
            streams(t + 1)(from)(to) = (currentState.owner,
              accOptimalStreamUnbound(planetDistances(from, to), currentState.size, t + 1),
              0)
          else {
            val lastStream = streams(t)(from)(to)
            streams(t + 1)(from)(to) = (lastStream._1, lastStream._2, lastStream._3 + 1)
          }
        }

        //if departure exists, then we should not add to strengths from this planet
        if (previousState.owner != NeutralPlanet)
          dep match {
            case None => {
              val destTurn = t + planetDistances(from, to) - 1
              if (destTurn <= MovesAhead) {
                val old = strengths(to)(destTurn)
                val streamState = streams(t)(from)(to)
                strengths(to)(destTurn) =
                  old.updated(previousState.owner, old(previousState.owner) + streamState._2(streamState._3))
              }
            }
            case _ =>
          }

      }
      previousStates(from) = currentState
      states(from)(t) = currentState
      statesUnderAttack(from)(t) = currentStateUnderAttack
    }
    (streams, strengths, states, statesUnderAttack, baselineAccGrowth)
  }
  
  def expectedOrDefault(v: Var) =
    if (v.max - v.min > LowConfidenceRange)
      DefaultValueIfVeryLowConfidence
    else
      v.expected

  def divideIntoFrontAndBack(currentStates: mutable.Map[PlanetId, FPlanet]): (mutable.Map[PlanetId, FPlanet], mutable.Map[PlanetId, FPlanet]) = {

    val myPlanets = currentStates.filter(_._2.owner == playerNumber)
    if (myPlanets.size == numberOfPlanets) {
      (mutable.Map[PlanetId, FPlanet](), currentStates)
    } else {
      val (front, back) = myPlanets.partition {
        x =>
          val (planet, state @ FPlanet(owner, size)) = x

          val enemies = planetsByDistance(planet).filter{
            p =>
              val pState = currentStates(p._1)
              (pState.owner != playerNumber) && (pState.owner != NeutralPlanet || pState.size < 50)
          }
          
          if (enemies.size > 0) {
            val closestEnemy = enemies.head
            //there is at most 1 planet, which is closer to the closest enemy
            myPlanets.count {
              y =>
                planetDistances(y._1, closestEnemy._1) < closestEnemy._2
            } < 2
          } else
            false
      }

      (front, back)
    }
  }
  
  /**
   * TODO wyslij statki tam, gdzie jest szansa na zmniejszenie growth pzeciwnika
   * 
   * TODO zmien strategie guerilla na atakowanie kombinacji sila+dystans
   */
  def redistributePlanets(population:  mutable.Map[PlanetId, mutable.Map[Turn, FPlanet]],
      strengths: Array[Array[Map[Player,Population]]],
      moves: List[TargetedMove]): List[TargetedMove] = {
    
    val usedPlanets = (for {
      move <- moves
      f <- move.flights if (f.turnDepart == 0)
    } yield (f.from -> f.size)).toMap
    
    val currentStates =
      for ((planet, turns) <- population)
        yield (planet, turns(0))
        
    val (front, back) = divideIntoFrontAndBack(currentStates)

    if (!front.isEmpty) {
      
      val powers = currentStates.groupBy(_._2.owner).map(x => (x._1, x._2.size.toDouble))
      val allPowers = powers.map(_._2).sum

      //calculate how much in need front planets are
      val frontSterngths = front.map {
        fp =>
          val (planet, state) = fp
          val futureState = population(planet)(MaxAttackTimeSpan)
          val sentShips = usedPlanets.get(planet).getOrElse(0)
          val str = strengths(planet)(MaxAttackTimeSpan)
          val strength =
            (if (futureState.owner == playerNumber) futureState.size else 0) - sentShips +
              str(playerNumber) - (str.filter(x => x._1 != playerNumber).map(x => x._2 * powers.getOrElse(x._1, 0.0) / allPowers).sum)
          (planet, strength)
      }

      val frontPlanetInNeed = frontSterngths.minBy(_._2)._1

      val supportMoves =
        for ((planet, state) <- back if (state.size >= 67 && !usedPlanets.contains(planet))) yield {

          val possibleFleets =
            for {
              (fs, d) <- FleetSizes if (fs != FleetType.HORDE)
              pop = (state.size * d).toInt
              if (Size.getSizeForNumber(state.size - pop) == Size.LARGE)
            } yield (fs, pop)
            
          val fleetSize = possibleFleets.maxBy(_._2)
          
          val closestFrontPlanet = planetsByDistance(planet).filter(x => front.contains(x._1)).head._1

          if (planetDistances(planet, closestFrontPlanet) >= 7) {
            //send reinforcements to planet in need
            val frontPlanetInRange = frontSterngths.filter(x => planetDistances(planet, x._1) <= MaxAttackTimeSpan)

            val target =
              if (frontPlanetInRange.isEmpty)
                frontPlanetInNeed
              else
                frontPlanetInRange.minBy(_._2)._1

            val flight = FFlight(planet, target, fleetSize._2,
              fleetSize._1, 0, planetDistances(planet, target) - 1)
            TargetedMove(playerNumber, target, List(flight), false)
          } else {
            //support closest
            val flight = FFlight(planet, closestFrontPlanet, fleetSize._2,
              fleetSize._1, 0, planetDistances(planet, closestFrontPlanet) - 1)
            TargetedMove(playerNumber, closestFrontPlanet, List(flight), false)
          }
        }

      def guerrillaTarget(planet: PlanetId): Option[(PlanetId, Distance)] = {
        val enemies = planetsByDistance(planet).filter(p =>
          currentStates(p._1).owner != playerNumber && currentStates(p._1).owner != NeutralPlanet)

        val enemiesInRange = enemies.filter(_._2 <= MaxAttackTimeSpan)

        val target = enemiesInRange match {
          case Nil => None
          case _ => {
            val enemiesByScore = enemiesInRange.map(x => (x._1, x._2 * 20 + currentStates(x._1).size)).sortBy(_._2)
            Some(enemiesByScore.head)
          }
        }
        target
      }

      val guerrillaMoves =
        for {
          (planet, state) <- front if (state.size >= 67 && !usedPlanets.contains(planet))
          if (Random.nextDouble < 0.05)
          target <- guerrillaTarget(planet)
        } yield {
          val flight = FFlight(planet, target._1, (state.size * 0.25).toInt,
            FleetType.SCOUTING, 0, target._2 - 1)
          TargetedMove(playerNumber, target._1, List(flight), false)
        }

      supportMoves.toList ::: guerrillaMoves.toList

    } else
      Nil
  }
  
  def rememberScheduledMoves(combined: List[TargetedMove]) = {
    for {
      move <- combined
      f <- move.flights
    } {
      if (f.turnDepart > 0) {
        val scheduledTurns = model.scheduledDepartures.getOrElseUpdate(f.from, mutable.Map[Turn, FFlight]())
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
    var usedPlanets = mutable.Set[PlanetId]()
    for {
      move <- moves
      if (!move.flights.exists(f => usedPlanets(f.from)))
    }
      yield {
        usedPlanets ++= move.flights.map(_.from)
        move
      }
  }
  
  def addStrengths(a: Map[Player,Population], b: Map[Player,Population]): Map[Player,Population] =
          Map(1 -> (a(1) + b(1)), 2 -> (a(2) + b(2)), 3 -> (a(3) + b(3)))
  
  def writeOutStrengths(output: PrintWriter, strengths: Array[Array[Map[Player,Population]]]) = {
    output.write("  {\n")
    
    for (planetId <- 0 until numberOfPlanets) {
      output.write("    \"" + planetId + "\": \"" + strengths(planetId)(MovesAhead) +
        "\",\n")
    }
    
    output.write("  },\n")
    output.flush()
  }

  def writeOutFrontBack(output: PrintWriter, population: mutable.Map[PlanetId, mutable.Map[Turn,FPlanet]]) = {
    output.write("  {\n")
    
        val currentStates =
      for ((planet, turns) <- population)
        yield (planet, turns(0))
    
    val (front, back) = divideIntoFrontAndBack(currentStates)
    
    for (planetId <- 0 until numberOfPlanets) {
      
      val value =
        if (currentStates(planetId).owner == playerNumber)
          if (front.contains(planetId)) "F" else "B"
        else " "
      
      output.write("    \"" + planetId + "\": \"" + value +
        "\",\n")
    }
    
    output.write("  },\n")
    output.flush()
  }
  
  def writeOutTargets(output: PrintWriter, filtered: List[TargetedMove], redistribution: List[TargetedMove]) = {
    output.write("  {\n")
    
    for (planetId <- 0 until numberOfPlanets) {

      def listContainsAsSource(lst: List[TargetedMove]): Boolean = {
        val sources =
          for {
            move <- lst
            flight <- move.flights if (flight.turnDepart == 0)
          } yield flight.from
        sources.exists(_ == planetId)
      }

      def listContainsAsTarget(lst: List[TargetedMove]): Boolean = {
        val targets =
          for {
            move <- lst
            flight <- move.flights if (flight.turnDepart == 0)
          } yield flight.to
        targets.exists(_ == planetId)
      }
      
      val status =
        if (listContainsAsSource(filtered)) "AS"
        else if (listContainsAsTarget(filtered)) "AT"
        else if (listContainsAsSource(redistribution)) "RS"
        else if (listContainsAsTarget(redistribution)) "RT"
        else " "
          
      output.write("    \"" + planetId + "\": \"" + status +
        "\",\n")
    }
    
    output.write("  },\n")
    output.flush()
  }
  
  def writeOutBalance(output: PrintWriter, sts: mutable.Map[PlanetId, mutable.Map[Turn,FPlanet]]) = {
    output.write("  {\n")

    for ((planetId, states) <- sts) {
      val cur = states(0)
      output.write("    \"" + planetId + "\": \"" + cur.size +
        "\",\n")
    }
    
    output.write("  },\n")
    output.flush()
  }

  def writeOutFuture(output: PrintWriter, sts: mutable.Map[PlanetId, mutable.Map[Turn,FPlanet]]) = {
    output.write("  {\n")

    for ((planetId, states) <- sts) {
      val x = for (i <- 0 to 15) yield states(i).size
      output.write("    \"" + planetId + "\": \"" + x.mkString(",") +
        "\",\n")
    }

    output.write("  },\n")
    output.flush()
  }
  
  /*
   * -------------- future model -------------
   */
  
  case class FFleet(owner: Owner, size: Population)
  
  case class FPlanet(owner: Owner, size: Population)
  
  case class FTargetPlanet(owner: Owner, size: Population, soft: Boolean)
  
  case class FFlight(from: PlanetId, to: PlanetId, size: Population,
      fleetType: FleetType, turnDepart: Turn, turnArrive: Turn) {
    override def toString = "from: " + from + ", to: " + to + ", size: " + size + ", depart: " + turnDepart + ", arrive: " + turnArrive 
  }
  
  case class TargetedMove(owner: Owner, target: PlanetId, flights: List[FFlight], scheduled: Boolean) {
    val timeSpan = flights match {
      case Nil => 0
      case _ => flights.map(x => x.turnArrive - x.turnDepart).max
    }
    
    val shipsSpent = flights.map(_.size).sum * (if (owner == NeutralPlanet) 2 else 1)
  }
  
  /**
   * Score is difference between accumulated growth after move
   * and accumulated growth before, divided by ships spent
   */
  def score(move: TargetedMove,
      streams: Array[Array[Array[(Player, IndexedSeq[Population], Int)]]],
      strengths: Array[Array[scala.collection.Map[Player,Population]]],
      states: Array[Array[FPlanet]],
      statesUnderAttack: Array[Array[FPlanet]],
      arrivals: mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]],
      departures: mutable.Map[PlanetId, mutable.Map[Turn,FFlight]],
      baselineAccGrowth: Int): Double = {
    (calculateAccGrowth(move, streams, strengths, states, statesUnderAttack, arrivals, departures, baselineAccGrowth) - baselineAccGrowth).toDouble / move.shipsSpent
  }
  
  case class FPlanetUpdate(prevState: FPlanet, departure: Option[FFleet], arrivals: Option[List[FFleet]])

  /**
   * TODO change how many ships can be sent where
   *
   * for simplicity right now departures and arrivals are not recalculated
   */
  def calculateAccGrowth(move: TargetedMove,
    streams: Array[Array[Array[(Player, IndexedSeq[Population], Int)]]],
    strengths: Array[Array[scala.collection.Map[Player, Population]]],
    states: Array[Array[FPlanet]],
    statesUnderAttack: Array[Array[FPlanet]],
    arrivals: mutable.Map[PlanetId, mutable.Map[Turn, List[FFleet]]],
    departures: mutable.Map[PlanetId, mutable.Map[Turn, FFlight]],
    baselineAccGrowth: Int): Int = {

    val newStates = Array.ofDim[FPlanet](numberOfPlanets, MovesAhead + 1)
    for {
      planet <- 0 until numberOfPlanets
      t <- 0 to MovesAhead
    } newStates(planet)(t) = states(planet)(t)

    val turnsToRecalculate = Array.fill(numberOfPlanets, MovesAhead + 1)(false)

    val streamsChanges = Array.ofDim[(Player, IndexedSeq[Population], Int)](MovesAhead + 1, numberOfPlanets, numberOfPlanets)
    val strengthsChanges = Array.ofDim[Map[Player, Population]](numberOfPlanets, MovesAhead + 1)

    val InitialStrength = Map(1 -> 0, 2 -> 0, 3 -> 0)

    for {
      planet: PlanetId <- 0 until numberOfPlanets
      t <- 0 to MovesAhead
    } strengthsChanges(planet)(t) = InitialStrength

    val arrs = Array.fill(numberOfPlanets)(mutable.Map[Turn, List[FFleet]]())

    var initialStates = Array.ofDim[FPlanet](numberOfPlanets)

    for ((planetId, states) <- model.timeline) {
      val currentState = states(model.turn)
      val exp = expectedOrDefault(currentState.current)
      val withoutGrowth =
        if (currentState.owner != NeutralPlanet) {
          if (exp >= 54)
            exp - 4
          else if (exp >= 22)
            exp - 2
          else
            exp - 1
        } else
          exp
      initialStates(planetId) = FPlanet(currentState.owner, withoutGrowth)
    }

    val moveFlights: mutable.Map[PlanetId, mutable.Map[Turn, FFlight]] = mutable.Map()

    for (flight <- move.flights) {
      val turnFlights = moveFlights.getOrElseUpdate(flight.from, mutable.Map())
      turnFlights(flight.turnDepart) = flight.copy(turnDepart=0, turnArrive=flight.turnArrive - flight.turnDepart)

      //mark states to recalculate
      turnsToRecalculate(flight.from)(flight.turnDepart) = true
      turnsToRecalculate(flight.to)(flight.turnArrive) = true
    }

    var currentScore = baselineAccGrowth

    for {
      t <- 0 to MovesAhead
      planet <- 0 until numberOfPlanets if turnsToRecalculate(planet)(t)
    } {

      val previousState =
        if (t == 0)
          initialStates(planet)
        else
          newStates(planet)(t - 1)

      def fleetFromFlight(flight: FFlight): FFleet = {
        val size = previousState.size + growth(playerNumber, previousState.size)

        val fleet = FFleet(playerNumber, flight.fleetType match {
          case FleetType.SCOUTING => (size * 0.25).toInt
          case FleetType.RAIDING => (size * 0.5).toInt
          case FleetType.ASSAULT => (size * 0.75).toInt
          case FleetType.HORDE => size
        })

        arrs(flight.to)(t + flight.turnArrive) = arrs(flight.to).get(t + flight.turnArrive) match {
          case None => List(fleet)
          case Some(l) => fleet :: l
        }
        fleet
      }

      //departure can come from the future scheduled move or current move
      val dep =
        if (previousState.owner == playerNumber) {
          departures.get(planet).flatMap(_.get(t).map(fleetFromFlight(_))) match {
            case None => moveFlights.get(planet).flatMap(_.get(t).map(fleetFromFlight(_)))
            case x: Some[FFleet] => x
          }
        } else
          None

      val arrivalsFromScheduledMoves = arrs(planet).get(t)
      val arrivalsFromGame = arrivals.get(planet).flatMap(_.get(t))
      val st = addStrengths(strengths(planet)(t), strengthsChanges(planet)(t))
      val arrivalsFromStrength =
        (for ((planet, size) <- st if (size > 0))
          yield FFleet(planet, size)).toList match {
          case Nil => None
          case x => Some(x)
        }

      val updatedState =
        resolveFPlanet(previousState,
          addArrivals(arrivalsFromScheduledMoves, arrivalsFromGame),
          dep)

      val updatedStateUnderAttack =
        resolveFPlanet(previousState,
          addArrivals(arrivalsFromScheduledMoves, addArrivals(arrivalsFromGame, arrivalsFromStrength)),
          dep)
          
      newStates(planet)(t) = updatedState
      
      val oldState = states(planet)(t)
      val oldStateUnderAttack = statesUnderAttack(planet)(t)
      if (oldState != updatedState) {

        if (t < MovesAhead)
          turnsToRecalculate(planet)(t + 1) = true

        if (oldState.owner == playerNumber && oldStateUnderAttack.owner == playerNumber)
          currentScore -= growth(oldState.size)

        if (updatedState.owner == playerNumber && updatedStateUnderAttack.owner == playerNumber)
          currentScore += growth(updatedState.size)

        if (oldState.owner != playerNumber && oldStateUnderAttack.owner != playerNumber &&
          oldState.owner != NeutralPlanet && oldStateUnderAttack.owner != NeutralPlanet)
          currentScore += growth(oldState.size)

        if (updatedState.owner != playerNumber && updatedStateUnderAttack.owner != playerNumber &&
          updatedState.owner != NeutralPlanet && updatedStateUnderAttack.owner != NeutralPlanet)
          currentScore -= growth(updatedState.size)

        //update strengths and streams
        for (to: PlanetId <- 0 until numberOfPlanets if (to != planet)) {

          if (t == 0 || streamsChanges(t)(planet)(to) == null) {
            val size = previousState.size + growth(previousState.owner, previousState.size)
            streamsChanges(t)(planet)(to) = (previousState.owner,
              accOptimalStreamUnbound(planetDistances(planet, to), size, t),
              0)
          }

          //update stream for t+1 if t < MovesAhead
          if (t < MovesAhead) {
            if (previousState.owner != updatedState.owner)
              streamsChanges(t + 1)(planet)(to) = (updatedState.owner,
                accOptimalStreamUnbound(planetDistances(planet, to), updatedState.size, t + 1),
                0)
            else {
              val lastStream = streamsChanges(t)(planet)(to)
              streamsChanges(t + 1)(planet)(to) = (lastStream._1, lastStream._2, lastStream._3 + 1)
            }

            val destTurn = t + planetDistances(planet, to) - 1
            if (destTurn <= MovesAhead) {
              turnsToRecalculate(to)(destTurn) = true
              
              //remove ald value
              val older = strengthsChanges(to)(destTurn)
              val streamStateOld = streams(t)(planet)(to)
              val old =
                if (streamStateOld._1 != NeutralPlanet) {
                  val updtd = older.updated(streamStateOld._1, older(streamStateOld._1) - streamStateOld._2(streamStateOld._3))
                  strengthsChanges(to)(destTurn) = updtd
                  updtd
                } else
                  older

              //add new value
              if (previousState.owner != NeutralPlanet)
                dep match {
                  case None => {
                    val streamState = streamsChanges(t)(planet)(to)
                    strengthsChanges(to)(destTurn) =
                      old.updated(previousState.owner, old(previousState.owner) + streamState._2(streamState._3))
                  }
                  case _ =>
                }
            }

          }

        }

      }

    }

    currentScore
  }
  
  case class PartialMoves(owner: Owner, moves: List[TargetedMove])
  
  /**
   * TODO for defensive actions consider looking for moves in earlier turns with
   * smaller size
   */
  def getPartialMoves(targets: mutable.Map[Int, mutable.Map[Int, FTargetPlanet]],
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
      
    def partialMovesForPlanet(t: Int, planet: Int, turnTargets: mutable.Map[Int,FTargetPlanet]): Option[PartialMoves] = {

	  @tailrec
	  def findTragetedMove(tgt: FTargetPlanet, planets: List[(Int, Int)], soFar: List[FFlight]): List[FFlight] = {
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
	              if ((!tgt.soft) || (Size.getSizeForNumber(ppop.size + growth(ppop.owner, ppop.size) - pop) == 
	                  Size.LARGE))  //must not affect planet size
	                //TODO this is not exactly right, because yhere might be incoming flights
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
                  findTragetedMove(FTargetPlanet(tgt.owner, tgt.size - bestFit._2, tgt.soft),
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
        partialMovesForPlanet(t - 1, planet, turnTargets) match {
          case x @ Some(moves) => {
            /**
             * if found move to capture planet from an enemy and if this enemy
             * is a neutral planet, then it is better to take it later from a
             * non-neutral enemy if it will be captured later by someone else
             */
            if (moves.owner == NeutralPlanet) {
              turnTargets.get(t) match {
                case Some(tgt) if tgt.owner != NeutralPlanet =>
                  findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => x._2 > MaxAttackTimeSpan || planetUsedInScheduledMoves(x._1)), Nil) match {
                    case Nil => None
                    case y => Some(PartialMoves(tgt.owner, List(TargetedMove(tgt.owner, planet, y, false))))
                  }
                case _ => x
              }
            } else if (moves.owner == playerNumber) {
              //if this is defensive move, the earliest move should win
              x
            } else {
              turnTargets.get(t) match {
                case None => x
                case Some(tgt) => {
                  if (moves.owner != tgt.owner) {
                    //owner has changed, so there is no point sending ships earlier
                    findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => x._2 > MaxAttackTimeSpan || planetUsedInScheduledMoves(x._1)), Nil) match {
                      case Nil => None
                      case y => Some(PartialMoves(tgt.owner, List(TargetedMove(tgt.owner, planet, y, false))))
                    }
                  } else {
                    if (Random.nextDouble < 0.3) {
                      findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => x._2 > MaxAttackTimeSpan || planetUsedInScheduledMoves(x._1)), Nil) match {
                        case Nil => x
                        case y => Some(PartialMoves(tgt.owner, TargetedMove(tgt.owner, planet, y, false) :: moves.moves))
                      }
                    } else
                      x
                  }
                }
              }
            }
          }
          case None => {
            turnTargets.get(t) match {
              case None => None
              case Some(tgt) =>
                findTragetedMove(tgt, planetsByDistance(planet).filterNot(x => x._2 > MaxAttackTimeSpan || planetUsedInScheduledMoves(x._1)), Nil) match {
                  case Nil => None
                  case x => Some(PartialMoves(tgt.owner, List(TargetedMove(tgt.owner, planet, x, false))))
                }
            }
          }
        }
      }
    }
    
    val partialMoves =
      (for {
        (planetId, turnTargets) <- targets
        partialMoves <- partialMovesForPlanet(MovesAhead, planetId, turnTargets)
        PartialMoves(pmOwner, pmMoves) = partialMoves
      }
        yield pmMoves).flatten
        
    val naggingTargets =
      for {
        (planetId, turnTargets) <- targets
        if (!partialMoves.exists(_.target == planetId))
      } yield {
        val adjustedTurnTargets =
          for ((trn, tgt) <- turnTargets if (tgt.owner != playerNumber))
            yield {
              (trn, tgt.copy(
                size =
                  if (tgt.size > 50)
                    tgt.size - 50
                  else if (tgt.size > 20)
                    tgt.size - 20
                  else tgt.size,
                soft = true
              ))
            }
        (planetId, adjustedTurnTargets)
      }
      
    val naggingPartialMoves =
      (for {
        (planetId, turnTargets) <- naggingTargets
        partialMoves <- partialMovesForPlanet(MovesAhead, planetId, turnTargets)
        PartialMoves(pmOwner, pmMoves) = partialMoves
      }
        yield pmMoves).flatten
      
    (partialMoves ++ naggingPartialMoves ++ scheduledMoves).toList
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
      
  def getArrivalsAndDepartures = {
    val arrivals = mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]]()
    val departures = mutable.Map[PlanetId, mutable.Map[Turn,FFlight]]()
    for {
      (planetId, ars) <- model.arrivals
      arrivalsOnPLanet = arrivals.getOrElseUpdate(planetId, mutable.Map())
      (turn, arList) <- ars
    } {
      arrivalsOnPLanet.put(model.modelToRelative(turn), arList.map(flight =>
        FFleet(flight.owner, expectedOrDefault(flight.current))))
    }
    
    for {
      (planetId, flights) <- model.scheduledDepartures
      departuresOnPLanet = departures.getOrElseUpdate(planetId, mutable.Map())
      (turn, f) <- flights if (turn >= model.turn)
    } {
      departuresOnPLanet(model.modelToRelative(turn)) = f
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
        
        //optimization for netral planets, which were recently won by enemy
        if (fleets.exists(_.owner == NeutralPlanet) &&
            !fleets.exists(_.owner == playerNumber)) {
          val won = fleets.maxBy(_.size)
          Some(FFleet(won.owner, (won.size - 2) max 2))
        } else {
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
   * Calculate balance for a planet. If balance is negative, the
   * returned value is number of ships needed to maintain the planet. If balance
   * is positive the returned value is number of ships which can be sent out.
   * 
   * Returns possible targets (how many ships need to be sent there)
   * for attack and defense actions.
   * [planet][turn]
   */
  def getBalanceAndTargets(arrivals: mutable.Map[PlanetId, mutable.Map[Turn, List[FFleet]]], 
      departures: mutable.Map[PlanetId, mutable.Map[Turn, FFleet]], 
      population:  mutable.Map[PlanetId, mutable.Map[Turn, FPlanet]]):
      (mutable.Map[Int, mutable.Map[Int, FPlanet]], mutable.Map[Int, mutable.Map[Int, FTargetPlanet]]) = {
    
    val balance = mutable.Map[PlanetId, mutable.Map[Turn, FPlanet]]()
    val target = mutable.Map[PlanetId, mutable.Map[Turn, FTargetPlanet]]()
    
    val noOpponents = !population.exists {
      x =>
        val owner = x._2(-1).owner
        owner != playerNumber && owner != NeutralPlanet
    }
    
    for ((planetId, _) <- model.timeline) {

      val planetArrivals = arrivals.getOrElse(planetId, mutable.Map[Turn, List[FFleet]]())
      val planetDepartures = departures.getOrElse(planetId, mutable.Map[Turn, FFleet]())
      val balances = mutable.Map[Int, FPlanet]()
      balance(planetId) = balances
      
      val targetTurns = mutable.Map[Turn, FTargetPlanet]()
      target(planetId) = targetTurns
      
      val turns = population(planetId)
      var planetOwnedInFuture = false
      var fightInFuture = false
      var need = 1
      
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

        val planetFleet = FFleet(cur.owner, prev.size + growth(prev.owner, prev.size) - departureSize)
        
        val fleets = turnArrivals match {
          case None => List(planetFleet)
          case Some(ars) => planetFleet :: ars
        }
        val grouped = fleets.groupBy(_.owner)
        
        val forces = (for (force <- grouped)
          yield (FFleet(force._1, force._2.map(_.size).sum))).toList
          
        if (forces.filter(_.owner != playerNumber).size > 1)
          fightInFuture = true;
          
        val maxOpponent: Population =
          forces.filter(_.owner != playerNumber) match {
            case Nil => 0
            case l =>
              l.reduce((x, y) => if (x.size > y.size) x else y).size
          }
        
        val playerForces = forces.find(_.owner == playerNumber).map(_.size).getOrElse(0)

        val balance =
          if (prev.owner != playerNumber) {
            //forces needed to tak eover a planet
            need = 1
            (maxOpponent + 2 - playerForces) max 0
          }
          else {
            if (cur.owner == playerNumber) {
              val b = cur.size - need
              need = (need - growth(need) + maxOpponent - (playerForces - (prev.size + growth(prev.owner, prev.size)))) max 1
              b
            } else {
              //defense
              need = prev.size
              (maxOpponent + 2 - playerForces) max 0
            }
          }
        
        balances(t) = FPlanet(cur.owner, balance)
        
        if (prev.owner == playerNumber) {
          if (balance < 0) {
            //defense
            targetTurns(t) = FTargetPlanet(prev.owner, -balance, false)
          } else {
            //friendly move, to improve development speed
            if (cur.size < 20)
              targetTurns(t) = FTargetPlanet(prev.owner, 20 - cur.size, true)
            else if (cur.size < 50)
              targetTurns(t) = FTargetPlanet(prev.owner, 50 - cur.size, true)
          }
        } else if (!planetOwnedInFuture && !fightInFuture &&
            (prev.owner != NeutralPlanet || (cur.size < 50 || noOpponents))) {
          //attack
          targetTurns(t) = FTargetPlanet(prev.owner, balance, false)
        }
        
      }
    }
    (balance, target)
  }
  
  
  def arrivalsCopy(arrivals: mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]]): mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]] = {
    val copy = mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]]()
    for ((planet, turns) <- arrivals) {
      val copyTurns = mutable.Map[Turn,List[FFleet]]()
      copy(planet) = copyTurns
      for ((t, fleet) <- turns)
        copyTurns(t) = fleet
    }
    copy
  }
  
  /**
   * This method adds adequate values to departures and arrival depending
   * on the scheduled flights
   * 
   * returns (futureBase, departures, arrivals)
   */
  def generateFuture(arrivals: mutable.Map[PlanetId, mutable.Map[Turn,List[FFleet]]],
      departures: mutable.Map[PlanetId, mutable.Map[Turn,FFlight]]) = {
    val population = mutable.Map[PlanetId, mutable.Map[Turn, FPlanet]]()
    val deps = mutable.Map[PlanetId, mutable.Map[Turn, FFleet]]()
    
    for ((planetId, states) <- model.timeline) {
      val currentState = states(model.turn)
      val turns = mutable.Map[Turn, FPlanet]()
      population(planetId) = turns
      
      val exp = expectedOrDefault(currentState.current)
      val withoutGrowth =
        if (currentState.owner != NeutralPlanet) {
          if (exp >= 54)
            exp - 4
          else if (exp >= 22)
            exp - 2
          else
            exp - 1
        } else
          exp
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
            
            val planetDeps = deps.getOrElseUpdate(planetId, mutable.Map[Turn, FFleet]())
            
            val fleet = FFleet(playerNumber, flight.fleetType match {
              case FleetType.SCOUTING => (size * 0.25).toInt
              case FleetType.RAIDING => (size * 0.5).toInt
              case FleetType.ASSAULT => (size * 0.75).toInt
              case FleetType.HORDE => size
            })
            planetDeps(t) = fleet
            
            val planetArs = arrivals.getOrElseUpdate(flight.to, mutable.Map[Turn, List[FFleet]]())
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