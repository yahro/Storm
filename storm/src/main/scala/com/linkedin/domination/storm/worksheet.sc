package com.linkedin.domination.storm

import CLP._
import StormCLP._
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import scala.collection._


object worksheet {

val storm = new Storm                             //> storm  : com.linkedin.domination.storm.Storm = com.linkedin.domination.storm
                                                  //| .Storm@38c52200
//(distance, population, turn)
storm.optimalStream(10, 50, 40)                   //> res0: scala.collection.Map[Int,Int] = Map()
storm.optimalStream(5, 50, 40)                    //> res1: scala.collection.Map[Int,Int] = Map(44 -> 1, 42 -> 3, 40 -> 37, 43 -> 
                                                  //| 2, 41 -> 10)

SortedMap[Int, Int]() ++ storm.optimalStreamUnbound(5, 170, 40)
                                                  //> res2: scala.collection.Map[Int,Int] = Map(40 -> 85, 41 -> 22, 42 -> 53, 43 -
                                                  //| > 14, 44 -> 4)
}