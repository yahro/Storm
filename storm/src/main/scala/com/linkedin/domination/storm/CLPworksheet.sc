package com.linkedin.domination.storm

import CLP._

object CLPworksheet {

  (RangeVar(10, 19) * 0.25) or (RangeVar(10, 19) * 0.5) or (RangeVar(10, 19) * 0.75)
                                                  //> res0: com.linkedin.domination.storm.CLP.Var = RangeVar(min=2, max=14)
  RangeVar(10, 19) - RangeVar(2, 14)              //> res1: com.linkedin.domination.storm.CLP.Var = RangeVar(min=-4, max=17)
}