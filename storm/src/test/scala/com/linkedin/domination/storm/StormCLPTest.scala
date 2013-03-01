package com.linkedin.domination.storm

import org.scalatest.FunSuite
import CLP._
import StormCLP._
import com.linkedin.domination.api.Size
import com.linkedin.domination.api.Move

class StormCLPTest  extends FunSuite {
  
  test("resolve battle #1") {
    val fleets = Map(3 -> Val(41), 1 -> Val(32))
    assert(resolvePastBatlle(fleets, true, 3).intersects(Val(32)))
  }
  
  test("resolve battle #2") {
    val fleets = Map(3 -> Val(41), 1 -> Val(16), 0 -> RangeVar(3, 19))
    assert(resolvePastBatlle(fleets, false, 3).intersects(Val(11)))
  }
  
  test("resolve battle #3") {
    val fleets = Map(3 -> Val(41), 1 -> Val(16), 0 -> RangeVar(20, 49))
    assert(resolvePastBatlle(fleets, false, 3).intersects(Val(36)))
  }

  
  test("planet abandoned and accupied by same player in one turn") {

    val pop0 = Val(30)
    val p0 = PlanetState(0,
      new Node(pop0, SizeRanges(Size.MEDIUM)),
      0,
      3,
      Size.MEDIUM :: Nil)
    p0.population.incoming ::= new Initial(p0, pop0)

    val pop1 = Val(41)
    val p1 = PlanetState(1,
      new Node(pop1, SizeRanges(Size.MEDIUM)),
      0,
      3,
      Size.MEDIUM :: Nil)
    p1.population.incoming ::= new Initial(p1, pop1)
    
    val f0 = new Flight(p0, Size.MEDIUM, 1, 3, 1)
    f0.set(ListVar(List(22, 30)))
    
    val f1 = new Flight(p1, Size.MEDIUM, 1, 3, 0)
    
    
    
  }
  
}