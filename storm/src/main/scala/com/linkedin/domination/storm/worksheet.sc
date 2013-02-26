package com.linkedin.domination.storm

import CLP._

object worksheet {


  (RangeVar(20, 49) - ListVar(List(25, 49))).intersects(RangeVar(1, Int.MaxValue))
                                                  //> res0: Boolean = true

}