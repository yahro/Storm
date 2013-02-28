package com.linkedin.domination.storm

import CLP._
import StormCLP._

object worksheet {

val fleets = Map(3 -> Val(41), 1 -> Val(16), 0 -> RangeVar(20, 49))
                                                  //> fleets  : scala.collection.immutable.Map[Int,Product with Serializable with 
                                                  //| com.linkedin.domination.storm.CLP.Var] = Map(3 -> Val(41), 1 -> Val(16), 0 -
                                                  //| > RangeVar(min=20, max=49))
val p = resolvePastBatlle(fleets, false, 3)       //> p  : com.linkedin.domination.storm.CLP.Var = RangeVar(min=18, max=65)

Val(41) - p                                       //> res0: com.linkedin.domination.storm.CLP.Var = RangeVar(min=0, max=23)

}