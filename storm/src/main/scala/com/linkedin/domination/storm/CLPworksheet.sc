package com.linkedin.domination.storm

import CLP._

object CLPworksheet {

	val val10 = Val(10)                       //> val10  : com.linkedin.domination.storm.CLP.Val = Val(10)
	val val20 = Val(20)                       //> val20  : com.linkedin.domination.storm.CLP.Val = Val(20)
	
	val r0to100 = RangeVar(0, 100)            //> r0to100  : com.linkedin.domination.storm.CLP.RangeVar = RangeVar(min=0, max=
                                                  //| 100)
	val r10to20 = RangeVar(10, 20)            //> r10to20  : com.linkedin.domination.storm.CLP.RangeVar = RangeVar(min=10, max
                                                  //| =20)
  val l1 = ListVar(List(10,11,12))                //> l1  : com.linkedin.domination.storm.CLP.ListVar = ListVar(List(10, 11, 12))
  val l2 = ListVar(List(11,12))                   //> l2  : com.linkedin.domination.storm.CLP.ListVar = ListVar(List(11, 12))

  val10.isMorePreciseThan(r0to100)                //> res0: Boolean = true
  val10.isMorePreciseThan(r10to20)                //> res1: Boolean = true
 
  val10.isMorePreciseThan(val20)                  //> res2: Boolean = false
  
  l1.isMorePreciseThan(l2)                        //> res3: Boolean = false
  
  l2.isMorePreciseThan(l1)                        //> res4: Boolean = true
  
  l2.isMorePreciseThan(l2)                        //> res5: Boolean = false
  
  l2.isMorePreciseThan(r0to100)                   //> res6: Boolean = true
  l2.isMorePreciseThan(r10to20)                   //> res7: Boolean = true
  
  l2.isMorePreciseThan(val10)                     //> res8: Boolean = false
  
  //arithmetic
  
  val10 + val20 - val10 == val20                  //> res9: Boolean = true
  
  r10to20 + val20 - val10                         //> java.lang.IllegalArgumentException: requirement failed: invalid range: min=-
                                                  //| 20, max=-30
                                                  //| 	at scala.Predef$.require(Predef.scala:214)
                                                  //| 	at com.linkedin.domination.storm.CLP$RangeVar.<init>(CLP.scala:13)
                                                  //| 	at com.linkedin.domination.storm.CLP$Val.$minus(CLP.scala:128)
                                                  //| 	at com.linkedin.domination.storm.CLP$RangeVar.$minus(CLP.scala:25)
                                                  //| 	at com.linkedin.domination.storm.CLPworksheet$$anonfun$main$1.apply$mcV$
                                                  //| sp(com.linkedin.domination.storm.CLPworksheet.scala:35)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at com.linkedin.domination.storm.CLPworksheet$.main(com.linkedin.dominat
                                                  //| ion.storm.CLPworksheet.scala:5)
                                                  //| 	at com.linkedin.domination.storm.CLPworkshe
                                                  //| Output exceeds cutoff limit.
  
}