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
  
}