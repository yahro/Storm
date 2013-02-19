package com.linkedin.domination.storm

object CLP {

  abstract trait Var {
    def +(v: Var): Var
    def -(v: Var): Var
//    def and(v: Var): Var
    def isMorePreciseThan(v: Var): Boolean
  }
  
  case class RangeVar(min: Int, max: Int) extends Var {
    require (min < max, "invalid range: min=" + min + ", max=" + max)
    
    override def +(variable: Var): Var = {
      variable match {
        case Val(_) => variable + this
        case RangeVar(min2, max2) => RangeVar(min + min2, max + max2)
        case ListVar(vals) => RangeVar(min + vals.head, max + vals.last)
      }
    }
    
    override def -(variable: Var): Var = {
      variable match {
        case Val(v) => RangeVar(min - v, max - v)
        case RangeVar(min2, max2) => RangeVar(min - max2, max - min2)
        case ListVar(vals) => RangeVar(min - vals.last, max - vals.head)
      }
    }
    
//    override def and(variable: Var): Var = {
//      variable match {
//        case x @ Val(v) if (v >= min && v <= max) => x
//        case RangeVar(min2, max2) => RangeVar(min max min2, max min max2)
//        case ListVar(vals) => ListVar(vals filter (x => x >= min && x <= max))
//        case _ => throw new CLPException(toString + " and " + variable + " is empty")
//      }
//    }
    
    override def isMorePreciseThan(variable: Var): Boolean = {
      variable match {
        case Val(_) => false
        case RangeVar(min2, max2) => min >= min2 && max <= max2 && (min > min2 || max < max2)
        //returns true if consecutive two elements on the list form a range, which
        //is less precise than this range
        case ListVar(vals) => {
          val (_, result) = vals.tail.foldLeft (vals.head, false) {
            (x, second) => {
              val (first, p) = x
              (second, p || 
                  (this.isMorePreciseThan(RangeVar(first, second)) || 
                   (this == RangeVar(first, second) && vals.size > 2)))
            }
          }
          result
        }
      }
    }
    
    override def toString = "RangeVar(min=" + min + ", max=" + max + ")"
  }
  
  /**
   * vals must be sorted
   */
  case class ListVar(vals: List[Int]) extends Var {
    require (vals == vals.sorted, "list must be sorted")
    require (vals == vals.distinct, "list can not contain repeated elements")
    require (vals.size > 1, "list must have at least two elements")
    
    override def +(variable: Var): Var = {
      variable match {
        case Val(_) => variable + this
        case RangeVar(_, _) => variable + this
        case ListVar(vals2) => ListVar(vals.foldLeft(List[Int]()){
          (list, x) => vals2.foldLeft(list){
            (l, y) => x + y :: l
          }
        }.distinct.sorted)
      }
    }
    
    override def -(variable: Var): Var = {
      variable match {
        case Val(v) => ListVar(vals.map(_ - v))
        case RangeVar(min, max) => RangeVar(vals.head - max, vals.last - min)
        case ListVar(vals2) => ListVar(vals.foldLeft(List[Int]()){
          (list, x) => vals2.foldLeft(list){
            (l, y) => x - y :: l
          }
        }.distinct.sorted)
      }
    }
    
//    override def and(variable: Var): Var = {
//      variable match {
//        case x @ Val(v) if vals.contains(v) => x
//        case x @ RangeVar(_, _) => x and this 
//        case ListVar(vals2) if (vals.containsSlice(vals2) || vals2.containsSlice(vals)) =>
//          ListVar(vals.intersect(vals2))
//        case _ => throw new CLPException(toString + " and " + variable + " is empty")
//      }
//    }
    
    override def isMorePreciseThan(variable: Var): Boolean = {
      variable match {
        case Val(_) => false
        case RangeVar(min, max) => vals.forall(x => x >= min && x <= max) &&
        	//there exist a gap
        	(min < vals.head ||  //gap in front
        	 max > vals.last ||  //gap at the end
        	 ! vals.sliding(2).forall { l =>   //gap in the middle
        	   val first :: second :: Nil = l
        	   second == first + 1
        	 })
        case ListVar(vals2) => vals != vals2 && vals2.containsSlice(vals)
      }
    }

    override def toString = "ListVar(" + vals + ")"
  }
  
  case class Val(v: Int) extends Var {
    override def +(variable: Var): Var = {
      variable match {
        case Val(v2) => Val(v + v2)
        case RangeVar(min, max) => RangeVar(v + min, v + max)
        case ListVar(vals) => ListVar(vals map (_ + v))
      }
    }

    override def -(variable: Var): Var = {
      variable match {
        case Val(v2) => Val(v - v2)
        case RangeVar(min, max) => RangeVar(v - max, v - min)
        case ListVar(vals) => ListVar((vals map (v - _)).reverse)
      }
    }
//    override def and(variable: Var): Var = {
//      if (this == variable)
//        this
//      else
//        throw new CLPException(toString + " and " + variable + " is empty")
//    }

    override def isMorePreciseThan(variable: Var): Boolean = {
      variable match {
        case Val(_) => false
        case _ => true
      }
    }
    override def toString = "Val(" + v + ")"
  }

  class Variable(v: Var) {
    private var hist: List[Var] = v :: Nil
    private var constraintSources: List[Constraint] = Nil
    private var constraintTargets: List[Constraint] = Nil
    def current = hist.head
    def set(v: Var) = {
      hist = v :: hist
      constraintSources map (_.sourceUpdate(v))
      constraintTargets map (_.targetUpdate(v))
    }
    def history = hist
  }

  abstract class Constraint(val source: Variable, val target: Variable) {
    def sourceUpdate(v: Var)
    def targetUpdate(v: Var)
  }
  
  class CLPException(msg: String) extends RuntimeException(msg)

}