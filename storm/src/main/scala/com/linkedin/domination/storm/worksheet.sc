package com.linkedin.domination.storm

import CLP._
import StormCLP._
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import scala.collection._


object worksheet {

val storm = new Storm                             //> storm  : com.linkedin.domination.storm.Storm = com.linkedin.domination.storm
                                                  //| .Storm@7afaa550
val t = 0                                         //> t  : Int = 0
                                             
val seq = storm.optimalStreamUnbound(6, 190, t)   //> seq  : IndexedSeq[com.linkedin.domination.storm.worksheet.storm.Population] 
                                                  //| = Vector(95, 49, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0
                                                  //| , 16, 0, 0, 0, 16, 0, 0, 0, 0, 0)
seq.size                                          //> res0: Int = 31
storm.MovesAhead + 1 - t                          //> res1: Int = 31
                       
seq.sum                                           //> res2: com.linkedin.domination.storm.worksheet.storm.Population = 240
           
}