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
      departures.get(planetId).foreach {
        event =>
          val flight = new Flight(lastTurnState,
                                  currentUniversePlanet.getSize(),
                                  Universe.getTimeToTravel(currentUniversePlanet,
                                      universe.getPlanetMap().get(event.getToPlanet())),
                                  lastTurnState.owner)
          val landingTurn = turn -1 + flight.duration
          val arrivals = flight :: arrivalsMap.getOrElseUpdate(landingTurn, Nil)
          arrivalsMap.update(landingTurn, arrivals)
      }
      
      //arrivals
      val arrivals = for (arrList <- arrivalsMap.get(turn)) yield arrList
      
      val playersOnPlanet = arrivals match {
        case None => lastTurnState.owner :: Nil
        case Some(l) => (lastTurnState.owner :: l.map(_.owner)).distinct
      }
      
      //create new turn
      val currentPlanetState = lastTurnState.createNextTurnState(currentUniversePlanet.getSize(), 
          currentUniversePlanet.getOwner())
      states(turn) = currentPlanetState
      
      playersOnPlanet.size match {
        case 1 => {
          //friendly mode
          
          //TODO take arrivals into account
          //TODO mount arrivals
          null
        }
        case _ => {
          //combat mode
          
          //TODO take combat into account
          //TODO mount arrivals
          null
        }
      }
      
      lastTurnState.population.applyMask
      currentPlanetState.population.applyMask

      lastTurnState.population.propagateBothWays
      currentPlanetState.population.propagateBothWays
     
    }
    
    for ((planetId, states) <- model.timeline) {
      states(turn - 1).population.propagateBothWays
      states(turn).population.propagateBothWays
    }
    
  }
  
  def initializeModel(universe: Universe) = {
    for ((id, planet) <- universe.getPlanetMap()){
      model.timeline(id.toInt) =
        mutable.Map(0 -> PlanetState.initialPlanetState(planet.getId(), planet.getOwner(), planet.getSize()))
    }
  }
  
}