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

class Storm extends Player {

  val getPlayerName = "Storm"
    
  var playerNumber = 0
  
  def initialize(playerNbr: java.lang.Integer) = {
    playerNumber = playerNbr
  }
  
  // ** AI constants **
  val MovesAhead = 32
  
  // ** Game state **
  var turn = 0

  case class Model(timeline: mutable.Map[Int, mutable.Map[Int, PlanetState]], arrivals: mutable.Map[Int, mutable.Map[Int, List[Flight]]])
  
  /**
   * score = accumulated growth of me - accumulated growth of opponents (based on futures)
   */
  
  /**
   * pomysl: trzymaj w pamieci historie gry, zeby mozna bylo zupdejtowac stany flot i planet gdy
   * jest nowa informacja: ladowanie z moja flota, zmiana statusu planety, walka:
   * - gdy planeta zmienila status,
   * - gdy byla walka
   * - gdy byly arrivals
   * 
   * 
   * pamietac tylko przeszlosc do terazniejszosci, przyszlos liczyc w kazdej turze od nowa
   * pamietac przyloty, ktore sie na pewno zdarza w przyszlosci...
   */
  
  val model: Model = Model(mutable.Map(), mutable.Map()) //initially empty model
  
  def makeMove(universe: Universe, events: java.util.List[Event]): java.util.List[Move] = {  
      
    //update model past
    if (model.timeline.isEmpty)
      initializeModel(universe)
    else
      updateModel(universe, events.toList);

    //update model future
    
    //TODO calculate moves
    
    //update turn
    turn += 1
      
    //TODO return moves
    List();
  }
  
  def updateModel(universe: Universe, events: List[Event]) = {
    
    val departures = events.filter(_.getEventType() == EventType.LAUNCH)
    					.map(x => (x.getFromPlanet(), x)).toMap
    
    //process events which can trigger better approximation:
    //- every turn if planet changed size it is exact value (if there were no combats)
    //- every turn if planet did not change size lower bound can be increased - this
    //	should be done automatically 
    //- landing of our fleet means we know exact values
      
    for ((planetId, states) <- model.timeline) {
      val lastTurnState = states(turn - 1)
      val currentUniversePlanet = universe.getPlanetMap().get(planetId)
      val arrivalsMap = model.arrivals.getOrElseUpdate(planetId, mutable.Map())
      
      //departures
      //first create flight
      val departure = departures.get(planetId).map {
        event =>
          //if this is our flight we know exact value
          val flight = new Flight(lastTurnState,
                                  event.getFleetSize(),
                                  Universe.getTimeToTravel(currentUniversePlanet,
                                      universe.getPlanetMap().get(event.getToPlanet())),
                                  lastTurnState.owner,
                                  event.getToPlanet())
          if (flight.owner == playerNumber)
            flight.set(Val(event.getSentShipCount()));
          val landingTurn = turn -1 + flight.duration
          val arrivalMap4Destination = model.arrivals.getOrElseUpdate(event.getToPlanet(), mutable.Map())
          val arrivals = flight :: arrivalMap4Destination.getOrElseUpdate(landingTurn, Nil)
          arrivalMap4Destination.update(landingTurn, arrivals)
          flight
      }

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
      val possiblyAbandoned = possiblyInvalid(lastTurnState.current - departure.map(_.current).getOrElse(Zero)).intersects(Zero)

      //arrivals
      val arrivalsOption = arrivalsMap.get(turn)
      
      val playersOnPlanet = arrivalsOption match {
        case None => lastTurnState.owner :: Nil
        case Some(l) => (lastTurnState.owner :: l.map(_.owner)).distinct
      }
      
      //create current turn planet state
      val currentPlanetState = playersOnPlanet.size match {
        case 1 => {
          //friendly mode
          
          val state = if (currentUniversePlanet.getOwner() != lastTurnState.owner) {
            //abandoned planet
            val planet = PlanetState(planetId,
        			new Node(Zero, SizeRanges(Size.SMALL)),
        			turn,
        			NeutralPlanet,
        			Size.SMALL :: Nil)
            planet.addIncoming(new Initial(planet, Zero))
            planet
          } else
            lastTurnState.createNextTurnState(currentUniversePlanet.getSize(), 
        		  currentUniversePlanet.getOwner(), departure.map(_.current),
        		  arrivalsOption.map(_.map(_.current).reduce(_ + _)))
        		  
            if (possiblyAbandoned)
              state.addOutgoing(new TakingOverPlanet(state, ListVar(List(0, 1))))

          for {
            arrivals <- arrivalsOption
            arrival <- arrivals
          } arrival.setTarget(state)
          
          state
        }
        case _ => {
          //combat mode

          //TODO if my fleet participated in combat I know exact values
          //TODO if I knew fleet sizes I could back-propagate this knowledge

          //otherwise calculate approximate numbers..

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
            case _ => victorForces + growthValForSize(sizesForVar(victorForces), victorForces, None, None)
          }
          
          val state = PlanetState(planetId,
              new Node(newPopulationIncludingGrowth, SizeRanges(currentUniversePlanet.getSize())),
              turn,
              currentUniversePlanet.getOwner(),
              currentUniversePlanet.getSize() :: Nil)
              
          val growth = state.owner match {
            case NeutralPlanet => None
            case _ => Some(new Growth(state, growthValForSize(sizesForVar(victorForces), 
            												  victorForces, None, None)))
          }
          
          state.population.incoming ::= new FromCombat(state, victorForces, arrivalsOption.get, lastTurnState)
          
          //attach outgoing edge from lastTurn, so that flight rooted at that state does not get rebalanced
          new BeforeCombat(lastTurnState)

          StormTesters.accuracyTester.foreach {
            tester =>
                tester.addMeasurmentForPlanet(planetId, state.population.current, state, states)
          }
          
          state
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
    
    for ((planetId, states) <- model.timeline) {
      states(turn - 1).population.propagateBothWays(MaxPropagationDepth)
      states(turn).population.propagateBothWays(MaxPropagationDepth)
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
    for ((id, planet) <- universe.getPlanetMap()){
      model.timeline(id.toInt) =
        mutable.Map(0 -> PlanetState.initialPlanetState(planet.getId(), planet.getOwner(), planet.getSize()))
    }
  }
  
}