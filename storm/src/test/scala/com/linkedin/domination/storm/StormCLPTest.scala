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
    val p1 = p0.createNextTurnState(Size.SMALL)
    assert(p1.current === initialVarForSize(Size.SMALL))
    p0.set(Val(15))
    assert(p1.current === Val(15))
  }

  test("backwards simple growth population - neutral planet") {
    val p0 = PlanetState.initialPlanetState(0, NeutralPlanet, Size.SMALL)
    val p1 = p0.createNextTurnState(Size.SMALL)
    p1.set(Val(15))
    assert(p0.current === Val(15))
  }
  
  test("forward simple growth population - player's planet") {
    val p0 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p1 = p0.createNextTurnState(Size.MEDIUM)
    assert(p1.current === Val(42))
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
    val f = new Flight(p00, Size.SMALL, 1)
    val p01 = p00.createNextTurnState(Size.MEDIUM)
    val p11 = p10.createNextTurnState(Size.MEDIUM)
    f.setTarget(p11)
    assert(p01.current === Val(32))
  }

  /**
   * Because fleet size is MEDIUM, it means that fleet size is ListVar(20, 30)
   * the size of planet in turn 1 is either 11 or 22, but because size of planet is MEDIUM
   * it means it has to be 22
   */
  test("create next turn with foreign Flight - exact fleet size unknown") {
    //planet names: p[id][turn]
    val p00 = PlanetState.initialPlanetState(0, 1, Size.MEDIUM)
    val p10 = PlanetState.initialPlanetState(1, 0, Size.MEDIUM)
    val f = new Flight(p00, Size.MEDIUM, 1)
    val p01 = p00.createNextTurnState(Size.MEDIUM)
    val p11 = p10.createNextTurnState(Size.MEDIUM)
    f.setTarget(p11)
    assert(f.current === ListVar(List(20, 30)))
    assert(p01.current === Val(22))
  }
  
}