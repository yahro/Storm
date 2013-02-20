package com.linkedin.domination.storm

import com.linkedin.domination.api.Player
import com.linkedin.domination.api.Universe
import com.linkedin.domination.api.Move
import com.linkedin.domination.api.Event
import scala.collection.JavaConversions._
import com.linkedin.domination.api.Planet
import com.linkedin.domination.api.Size
import scala.collection._

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

  abstract class Population {
    def +(population: Population): Population
  }
  
  case class Estimate(min: Int, max: Int) extends Population {
    override def +(population: Population): Population = {
      population match {
        case Exact(value) => Estimate(value + min, value + max)
        case Estimate(mi, ma) => Estimate(min + mi, max + ma) 
      }
    }
  }
  
  case class Exact(value: Int) extends Population {
    override def +(population: Population): Population = {
      population match {
        case Exact(v) => Exact(value + v)
        case Estimate(min, max) => Estimate(value + min, value + max)
      }
    }
  }
  
  //TODO: zamiast tego: class PopulationChange??, incoming and changes in population state
  abstract class ImprovablePopulation(var populationState: List[Population],
      var improvee: List[(ImprovablePopulation, ImprovablePopulation) => Unit]) {
    def current: Population //TODO
    def setCurrent(population: Population)//TODO
    def history: List[Population]
  }

  case class Model(timeline: mutable.Map[Int, Vector[PlanetState]])
  case class Flight(from: Int, to: Int, size: Population, departureTurn: Int, arrivalTurn: Int)
  case class PlanetState(population: Population, arrivals: List[Flight], 
      departures: List[Flight], owner: Int, relativeSize: Option[Size])
  
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
      updateCurrentPopulations(universe, events.toList);
      
    //update model future
    
    //TODO calculate moves
    
    //TODO events happened in last turn
    
    //update turn
    turn += 1
      
    //TODO return moves
    List();
  }
  
  def updateCurrentPopulations(universe: Universe, events: List[Event]) = {
    for ((planetId, states) <- model.timeline) {
      val lastTurnState = states(turn - 1)
      
      //instead of this approach
      //calculate planet state change:
      //owner changed
      //size changed etc
      //arrivals
      //departures
      //and use pattern matching
      
      //najpierw rospisac sobie jakie sa mozliwosci
      
      lastTurnState.population match {
        //if planet population is not known yet
        case Estimate(eMin, eMax) => {
	      //first let's check if relative size has changed,
	      //in that case we know exact size of the planet
          val planetFromMap = universe.getPlanetMap().get(planetId)
          //TODO also check there was no fight here
	      if (Some(planetFromMap.getSize()) != lastTurnState.relativeSize) {
	        if (planetFromMap.getSize() == Size.MEDIUM)
	          model.timeline(planetId) = states :+
	            PlanetState(Exact(20), List(), List(), planetFromMap.getOwner(), None)
	        else if (planetFromMap.getSize() == Size.LARGE)
	          model.timeline(planetId) = states :+
	            PlanetState(Exact(50), List(), List(), planetFromMap.getOwner(), None)
	      } else {
	        //TODO
	        model.timeline(planetId) = states :+ lastTurnState  //add growth
	      }
        }
        case Exact(value) => {
          //TODO
	        model.timeline(planetId) = states :+ lastTurnState  //add growth
        } 
      }
    }
    
    
    //TODO
  }
  
  def initializeModel(universe: Universe) = {
    for ((id, planet) <- universe.getPlanetMap()){
      model.timeline(id.toInt) = Vector(initialState(planet))
    }
  }

  def initialState(planet: Planet): PlanetState = {
    planet.getOwner() match {
      case NeutralPlanet =>
        PlanetState(estimateNeutralPlanetInitialPopulation(planet),
            List(), List(), NeutralPlanet, Some(planet.getSize()))
      case player => PlanetState(Exact(planet.getPopulation), List(), List(), player, None)
    }
  }
  
  def estimateNeutralPlanetInitialPopulation(planet: Planet): Estimate = {
    if (planet.getSize() == Size.SMALL)
      Estimate(1, 19)
    else if (planet.getSize() == Size.MEDIUM)
      Estimate(20, 49)
    else Estimate(50, 90)  //TODO confirm it should be 90
  }
  
  def growth(population: Population): Population = {
    population match {
      case Exact(value) => {
        if (value < 20)
          Exact(1)
        else if (value < 50)
          Exact(2)
        else
          Exact(4)
      }
      case Estimate(eMin, eMax) => {
        val minGrowth = growth(Exact(eMin)).asInstanceOf[Exact]
        val maxGrowth = growth(Exact(eMax)).asInstanceOf[Exact]
        if (minGrowth == maxGrowth)
          minGrowth
        else
          Estimate(minGrowth.value, maxGrowth.value)
      }
    }
  }
  
}