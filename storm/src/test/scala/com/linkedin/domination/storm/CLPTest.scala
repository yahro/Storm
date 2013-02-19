package com.linkedin.domination.storm

import org.scalatest.FunSuite
import CLP._

class CLPTest  extends FunSuite {
  
  /* 
   * Val
   */  
  
  test("Val + Val") {
    assert(Val(2) + Val(3) === Val(5))
  }

  test("Val + RangeVar") {
    assert(Val(2) + RangeVar(3, 6) === RangeVar(3 + 2, 6 + 2))
  }

  test("Val + ListVar") {
    assert(Val(2) + ListVar(List(13, 23, 61)) === ListVar(List(13 + 2, 23 + 2, 61 + 2)))
  }
  
  test("Val - Val") {
    assert(Val(21) - Val(3) === Val(18))
  }

  test("Val - RangeVar") {
    assert(Val(22) - RangeVar(3, 6) === RangeVar(22 - 6, 22 - 3)) 
  }

  test("Val - ListVar") {
    assert(Val(122) - ListVar(List(13, 23, 61)) === ListVar(List(122 - 13, 122 - 23, 122 - 61).reverse)) 
  }
  
  test("Val isMorePreciseThan Val") {
    assert(Val(2).isMorePreciseThan(Val(2)) === false)
    assert(Val(20).isMorePreciseThan(Val(2)) === false)
  }
  
  test("Val isMorePreciseThan RangeVar") {
    assert(Val(2).isMorePreciseThan(RangeVar(2, 3)) === true)
    assert(Val(20).isMorePreciseThan(RangeVar(2, 3)) === true)
  }

  test("Val isMorePreciseThan ListVar") {
    assert(Val(2).isMorePreciseThan(ListVar(List(2, 23, 61))) === true)
    assert(Val(20).isMorePreciseThan(ListVar(List(2, 23, 61))) === true)
  }
  
  /* 
   * RangeVar
   */  

  test("RangeVar + Val") {
    assert(RangeVar(3, 6) + Val(2) === RangeVar(3 + 2, 6 + 2))
  }

  test("RangeVar + RangeVar") {
    assert(RangeVar(3, 6) + RangeVar(13, 66) === RangeVar(3 + 13, 6 + 66))
  }

  test("RangeVar + ListVar") {
    assert(RangeVar(13, 66) + ListVar(List(13, 23, 61)) === RangeVar(13 + 13, 66 + 61))
  }
  
  test("RangeVar - Val") {
    assert(RangeVar(13, 66) - Val(3) === RangeVar(13 - 3, 66 - 3))
  }

  test("RangeVar - RangeVar") {
    assert(RangeVar(13, 66) - RangeVar(3, 6) === RangeVar(13 - 6, 66 - 3)) 
  }

  test("RangeVar - ListVar") {
    assert(RangeVar(113, 166) - ListVar(List(13, 23, 61)) === RangeVar(113 - 61, 166 - 13)) 
  }
  
  test("RangeVar isMorePreciseThan Val") {
    assert(RangeVar(13, 66).isMorePreciseThan(Val(12)) === false)
    assert(RangeVar(13, 66).isMorePreciseThan(Val(2)) === false)
  }
  
  test("RangeVar isMorePreciseThan RangeVar") {
    assert(RangeVar(13, 66).isMorePreciseThan(RangeVar(13, 66)) === false)
    assert(RangeVar(13, 66).isMorePreciseThan(RangeVar(12, 66)) === true)
    assert(RangeVar(13, 66).isMorePreciseThan(RangeVar(13, 67)) === true)
    assert(RangeVar(13, 66).isMorePreciseThan(RangeVar(1, 200)) === true)
    assert(RangeVar(13, 66).isMorePreciseThan(RangeVar(15, 16)) === false)
  }

  test("RangeVar isMorePreciseThan ListVar") {
    assert((RangeVar(13, 66).isMorePreciseThan(ListVar(List(23, 61))) === false))
    assert((RangeVar(13, 14).isMorePreciseThan(ListVar(List(13, 14))) === false))
    assert((RangeVar(13, 14).isMorePreciseThan(ListVar(List(10, 13, 14))) === true))
  }
  
  /* 
   * ListVar
   */  

  test("ListVar + Val") {
    assert(ListVar(List(10, 13, 14)) + Val(2) === ListVar(List(10 + 2, 13 + 2, 14 + 2)))
  }

  test("ListVar + RangeVar") {
    assert(ListVar(List(13, 23, 61)) + RangeVar(13, 66) === RangeVar(13 + 13, 66 + 61))
  }

  test("ListVar + ListVar") {
    assert(ListVar(List(10, 13)) + ListVar(List(5, 10)) === ListVar(List(10 + 5, 10 + 10, 13 + 5, 13 + 10).sorted))
  }
  
  test("ListVar - Val") {
    assert(ListVar(List(10, 13, 14)) - Val(3) === ListVar(List(10 - 3, 13 - 3, 14 - 3)))
  }

  test("ListVar - RangeVar") {
    assert(ListVar(List(10, 13, 14)) - RangeVar(3, 6) === RangeVar(10 - 6, 14 - 3)) 
  }

  test("ListVar - ListVar") {
    assert(ListVar(List(10, 13)) - ListVar(List(3, 5)) === ListVar(List(10 - 3, 10 - 5, 13 - 3, 13 - 5).sorted))
  }
  
  test("ListVar isMorePreciseThan Val") {
    assert(ListVar(List(10, 13)).isMorePreciseThan(Val(10)) === false)
    assert(ListVar(List(10, 13)).isMorePreciseThan(Val(2)) === false)
  }
  
  test("ListVar isMorePreciseThan RangeVar") {
    assert(ListVar(List(10, 13)).isMorePreciseThan(RangeVar(13, 66)) === false)
    assert(ListVar(List(10, 13)).isMorePreciseThan(RangeVar(1, 66)) === true)
    assert(ListVar(List(10, 13)).isMorePreciseThan(RangeVar(11, 12)) === false)
  }

  test("ListVar isMorePreciseThan ListVar") {
    assert((ListVar(List(1, 23, 61)).isMorePreciseThan(ListVar(List(23, 61))) === false))
    assert((ListVar(List(23, 61)).isMorePreciseThan(ListVar(List(23, 61))) === false))
    assert((ListVar(List(23, 61)).isMorePreciseThan(ListVar(List(23, 61, 88))) === true))
  }
  
  
}