package com.linkedin.domination.storm

import CLP._
import StormCLP._

object worksheet {

Val(42) + ListVar(List(0, 2))                     //> res0: com.linkedin.domination.storm.CLP.Var = ListVar(List(42, 44))

ListVar(List(0, 10, 20)) + ListVar(List(0, 1, 2)) //> res1: com.linkedin.domination.storm.CLP.Var = ListVar(List(0, 1, 2, 10, 11, 
                                                  //| 12, 20, 21, 22))

}