package com.linkedin.domination.storm

import org.scalatest.FunSuite
import CLP._
import StormCLP._
import com.linkedin.domination.api.Size
import com.linkedin.domination.api.Move

class StormCLPTest  extends FunSuite {
  
  //TODO test negative values, make sure they don't occur
  
  test("forward simple growth population - neutral planet") {
    val p0 = PlanetState.initialPlanetState(0, NeutralPlanet, Size.SMALL)
    val p1 = p0.createNextTurnState(Size.SMALL, NeutralPlanet)
    
    p0.population.propagateBothWays
    p1.population.propagateBothWays
    
    assert(p1.current === initialVarForSize(Size.SMALL))
    p0.set(Val(15))

    p0.population.propagateBothWays

    assert(p1.current === Val(15))
  }

  test("backwards simple growth population - neutral planet") {
    val p0 = PlanetState.initialPlanetState(0, NeutralPlanet, Size.SMALL)
    val p1 = p0.createNextTurnState(Size.SMALL, NeutralPlanet)
    p1.set(Val(15))
    assert(p0.current === Val(15))
  }
  
  test("forward simple growth population - player's planet") {
    val owner = 1
    val p0 = PlanetState.initialPlanetState(0, owner, Size.MEDIUM)
    val p1 = p0.createNextTurnState(Size.MEDIUM, owner)
    assert(p1.current === Val(42))
  }

  test("forward growth population multiple turns - player's planet MEDIUM to LARGE") {
    val owner = 1
    var planets = PlanetState.initialPlanetState(0, owner, Size.MEDIUM) :: Nil
    val f = new Flight(planets.head, Size.MEDIUM, 1, owner, 1)
    for (i <- 22 to 48 by 2) {
      planets = planets.head.createNextTurnState(Size.MEDIUM, owner) :: planets
      planets.head.population.applyMask
      planets.head.population.propagateBothWays
      planets.tail.head.population.applyMask
      planets.tail.head.population.propagateBothWays
      assert(planets.head.current === Val(i))
    }
    planets = planets.head.createNextTurnState(Size.LARGE, owner) :: planets
    planets.head.population.applyMask
    planets.head.population.propagateBothWays
    planets.tail.head.population.applyMask
    planets.tail.head.population.propagateBothWays
    assert(planets.head.current === Val(50))
  }
  
  /**
   * Because fleet size is SMALL, and original planet size is 40, we know that
   * it must be equal to 10. Thus, population subtracted by fleet size is 30.
   * Next turn population is 32. 
   */
  test("create next turn with foreign Flight - value implied by size of fleet") {
    //planet names: p[id][turn]
    val p00 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p10 = PlanetState.initialPlanetState(1, 0, Size.MEDIUM)
    val f = new Flight(p00, Size.SMALL, 1, 1, 1)
    
    val p01 = p00.createNextTurnState(Size.MEDIUM, 1)
    val p11 = p10.createNextTurnState(Size.MEDIUM, 0)
    f.setTarget(p11)

    p00.population.propagateBothWays
    p10.population.propagateBothWays
    
    assert(p01.current === Val(32))
  }

  /**
   * Because fleet size is MEDIUM, it means that fleet size is ListVar(20, 30)
   * the size of planet in turn 1 is either 11 or 22, but because size of planet is MEDIUM
   * it means it has to be 22, which means in turn, that fleet size must not have been 30
   * but only 20
   */
  test("create next turn with foreign Flight - exact fleet size implied by planet size MEDIUM") {
    //planet names: p[id][turn]
    val p00 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p10 = PlanetState.initialPlanetState(1, 0, Size.MEDIUM)
    val f = new Flight(p00, Size.MEDIUM, 1, 1, 1)
    
    val p01 = p00.createNextTurnState(Size.MEDIUM, 1)
    val p11 = p10.createNextTurnState(Size.MEDIUM, 0)

    f.setTarget(p11)
    p11.population.incomingChanged
    
    p01.population.applyMask
    p11.population.applyMask

    p00.population.propagateBothWays
    p10.population.propagateBothWays
    p01.population.propagateBothWays
    p11.population.propagateBothWays
    
    assert(f.current === Val(20))
    assert(p01.current === Val(22))
  }

  /**
   * Because fleet size is MEDIUM, it means that fleet size is ListVar(20, 30)
   * the size of planet in turn 1 is either 11 or 22, but because size of planet is SMALL
   * it means it has to be 12, which means in turn, that fleet size must not have been 20
   * but only 30
   */
  test("create next turn with foreign Flight - exact fleet size implied by planet size SMALL") {
    //planet names: p[id][turn]
    val p00 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p10 = PlanetState.initialPlanetState(1, 0, Size.MEDIUM)
    val f = new Flight(p00, Size.MEDIUM, 1, 1, 1)
    
    val p01 = p00.createNextTurnState(Size.SMALL, 1)
    val p11 = p10.createNextTurnState(Size.LARGE, 0)
    
    f.setTarget(p11)

    p01.population.applyMask
    p11.population.applyMask

    p00.population.propagateBothWays
    p10.population.propagateBothWays
    p01.population.propagateBothWays
    p11.population.propagateBothWays
    
    assert(f.current === Val(30))
    assert(p01.current === Val(12))
  }

  /**
   * Because fleet size is SMALL, it means that fleet size is Val(10)
   * the size of planet in turn 1 is either 32
   */
  test("create next turn with foreign Flight - fleet is SMALL") {
    //planet names: p[id][turn]
    val p00 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p10 = PlanetState.initialPlanetState(1, 0, Size.MEDIUM)
    val f = new Flight(p00, Size.SMALL, 1, 1, 1)
    
    val p01 = p00.createNextTurnState(Size.SMALL, 1)
    val p11 = p10.createNextTurnState(Size.LARGE, 0)
    
    f.setTarget(p11)

    p01.population.applyMask
    p11.population.applyMask

    p00.population.propagateBothWays
    p10.population.propagateBothWays
    p01.population.propagateBothWays
    p11.population.propagateBothWays
    
    assert(f.current === Val(10))
    assert(p01.current === Val(32))
  }
  
  test("forward simple growth population - player abandoned planet") {
    val owner = 1
    var planets = PlanetState.initialPlanetState(0, owner, Size.MEDIUM) :: Nil
    val f = new Flight(planets.head, Size.MEDIUM, 1, owner, 1)
    for (i <- 22 to 48 by 2) {
      planets = planets.head.createNextTurnState(Size.MEDIUM, owner) :: planets
      planets.head.population.applyMask
      planets.head.population.propagateBothWays
      planets.tail.head.population.applyMask
      planets.tail.head.population.propagateBothWays
      assert(planets.head.current === Val(i))
    }
    planets = planets.head.createNextTurnState(Size.LARGE, owner) :: planets
    planets.head.population.applyMask
    planets.head.population.propagateBothWays
    planets.tail.head.population.applyMask
    planets.tail.head.population.propagateBothWays
    assert(planets.head.current === Val(50))
    
    val migration = new Flight(planets.head, Size.LARGE, 1, owner, 1)
    planets = planets.head.createNextTurnState(Size.SMALL, NeutralPlanet) :: planets
    planets.head.population.applyMask
    planets.head.population.propagateBothWays
    planets.tail.head.population.applyMask
    planets.tail.head.population.propagateBothWays

    assert(planets.head.current === Val(0))
    assert(planets.head.owner === NeutralPlanet)
    
  }
  
  
}