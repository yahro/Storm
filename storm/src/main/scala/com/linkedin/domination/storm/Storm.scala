package com.linkedin.domination.storm

import com.linkedin.domination.api.Player
import com.linkedin.domination.api.Universe
import com.linkedin.domination.api.Move
import com.linkedin.domination.api.Event
import scala.collection.JavaConversions._
import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Size
import scala.collection._
import CLP._
import StormCLP._

//TODO: na podstawie romiaru mozna zawezac przedzialy...


class Storm extends Player {

  val getPlayerName = "Storm"
    
  var playerNumber = 0
  
  def initialize(playerNbr: java.lang.Integer) = {
    playerNumber = playerNbr
  }
  
  // ** Game constants **
  val InitialPlayerPopulation = 40
  val NeutralPlanet = 0
  
  // ** AI constants **
  val MovesAhead = 32
  
  // ** Game state **
  var turn = 0

  case class Model(timeline: mutable.Map[Int, Vector[PlanetState]])
  
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
  
  val model: Model = Model(mutable.Map()) //initially empty model
  
  def makeMove(universe: Universe, events: java.util.List[Event]): java.util.List[Move] = {  
      
    //update model past
    if (model.timeline.isEmpty)
      initializeModel(universe)
    else
      updateModel(universe, events.toList);
      
    //update model future
    
    //TODO calculate moves
    
    //TODO events happened in last turn
    
    //update turn
    turn += 1
      
    //TODO return moves
    List();
  }
  
  def updateModel(universe: Universe, events: List[Event]) = {
    
      //process events which can trigger better approximation:
      //- every turn if planet changed size it is exact value
      //- every turn if planet did not change size lower bound can be increased
      //- landing of our fleet means we know exact values
    for ((planetId, states) <- model.timeline) {
      val lastTurnState = states(turn - 1)
      //TODO
      
    }
    
    
    //TODO create new states for this turn
  }
  
  def initializeModel(universe: Universe) = {
    for ((id, planet) <- universe.getPlanetMap()){
      model.timeline(id.toInt) = Vector(initialState(planet))
    }
  }

  def initialState(planet: Planet): PlanetState = {
    planet.getOwner() match {
      case NeutralPlanet => {
    	val population = if (planet.getSize() == Size.SMALL)
        			  RangeVar(1, 19)
        			else if (planet.getSize() == Size.MEDIUM)
        			  RangeVar(20, 49)
        			else RangeVar(50, 90)
        val planetState = PlanetState(planet.getId(),
        			new VariableNode(population),
        			0,
        			NeutralPlanet,
        			planet.getSize())
        planetState.population.incoming ::= new Growth(planetState, population)
        planetState
      }
      case player => {
        val population = Val(40)
        val planetState = PlanetState(planet.getId(),
        			new VariableNode(population),
        			0,
        			player,
        			planet.getSize())
        planetState.population.incoming ::= new Growth(planetState, population)
        planetState
      }
    }
  }
  
  
}