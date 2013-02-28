package com.linkedin.domination.storm

import CLP._

object worksheet {

Val(24) - ListVar(List(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29))
                                                  //> java.lang.IllegalArgumentException: requirement failed
                                                  //| 	at scala.Predef$.require(Predef.scala:202)
                                                  //| 	at com.linkedin.domination.storm.CLP$ListVar.<init>(CLP.scala:199)
                                                  //| 	at com.linkedin.domination.storm.CLP$Val.$minus(CLP.scala:73)
                                                  //| 	at com.linkedin.domination.storm.worksheet$$anonfun$main$1.apply$mcV$sp(
                                                  //| com.linkedin.domination.storm.worksheet.scala:7)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at com.linkedin.domination.storm.worksheet$.main(com.linkedin.domination
                                                  //| .storm.worksheet.scala:5)
                                                  //| 	at com.linkedin.domination.storm.worksheet.main(com.linkedin.domination.
                                                  //| storm.worksheet.scala)


}