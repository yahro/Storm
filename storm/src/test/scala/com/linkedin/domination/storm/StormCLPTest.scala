package com.linkedin.domination.storm

import org.scalatest.FunSuite
import CLP._
import StormCLP._
import com.linkedin.domination.api.Size

class StormCLPTest  extends FunSuite {
  
  test("forward simple growth population") {
    val p0 = PlanetState(0, new VariableNode(RangeVar(10, 20)), 0, 1, Size.SMALL)
    val p1 = PlanetState(0, new VariableNode(RangeVar(11, 21)), 1, 1, Size.SMALL)
    val initial = new InitialState(p0, RangeVar(10, 20))
    p0.population.incoming ::= initial
    val growth = new Growth(p0, p1, Val(1))
    p0.population.outgoing ::= growth
    p1.population.incoming ::= growth
    p0.population.set(Val(15))
    assert(growth.current === Val(16))
    assert(p1.population.current === Val(16))
  }

  test("backwards simple growth population") {
    val p0 = PlanetState(0, new VariableNode(RangeVar(10, 20)), 0, 1, Size.SMALL)
    val p1 = PlanetState(0, new VariableNode(RangeVar(11, 21)), 1, 1, Size.SMALL)
    val initial = new InitialState(p0, RangeVar(10, 20))
    p0.population.incoming ::= initial
    val growth = new Growth(p0, p1, Val(1))
    p0.population.outgoing ::= growth
    p1.population.incoming ::= growth
    p1.population.set(Val(16))
    assert(growth.current === Val(16))
    assert(p0.population.current === Val(15))
  }
 
}