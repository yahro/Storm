package com.linkedin.domination.storm

import CLP._
import StormCLP._
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import scala.collection._


object worksheet {

val storm = new Storm                             //> storm  : com.linkedin.domination.storm.Storm = com.linkedin.domination.storm
                                                  //| .Storm@2e716cb7
val t = 0                                         //> t  : Int = 0
                                             
val seq = storm.optimalStreamUnbound(6, 190, t)   //> seq  : IndexedSeq[com.linkedin.domination.storm.worksheet.storm.Population] 
                                                  //| = Vector(95, 49, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0
                                                  //| , 16, 0, 0, 0, 0, 0, 0, 0)

seq.size                                          //> res0: Int = 29
storm.MovesAhead + 1 - t                          //> res1: Int = 29
                       
seq.sum                                           //> res2: com.linkedin.domination.storm.worksheet.storm.Population = 224


val accSeq = storm.accumulateStream(seq)          //> accSeq  : IndexedSeq[com.linkedin.domination.storm.worksheet.storm.Populatio
                                                  //| n] = Vector(95, 144, 144, 144, 144, 160, 160, 160, 160, 176, 176, 176, 176, 
                                                  //| 192, 192, 192, 192, 208, 208, 208, 208, 224, 224, 224, 224, 224, 224, 224, 2
                                                  //| 24)
}