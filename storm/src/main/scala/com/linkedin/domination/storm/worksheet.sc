package com.linkedin.domination.storm

import CLP._
import StormCLP._
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import scala.collection._


object worksheet {

val storm = new Storm                             //> storm  : com.linkedin.domination.storm.Storm = com.linkedin.domination.storm
                                                  //| .Storm@59d0d45b
val t = 0                                         //> t  : Int = 0
                                             
val seq = storm.optimalStreamUnbound(6, 190, 23)  //> seq  : IndexedSeq[com.linkedin.domination.storm.worksheet.storm.Population] 
                                                  //| = Vector(95, 0, 0, 0, 0, 0)

val bbb = storm.accOptimalStreamUnbound(6, 190, 18)
                                                  //> bbb  : IndexedSeq[com.linkedin.domination.storm.worksheet.storm.Population] 
                                                  //| = Vector(0, 0, 0, 0, 0, 95, 144, 184, 195, 198, 200)

seq.size                                          //> res0: Int = 6
storm.MovesAhead + 1 - t                          //> res1: Int = 29
                       
seq.sum                                           //> res2: com.linkedin.domination.storm.worksheet.storm.Population = 95


}