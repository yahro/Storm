package com.linkedin.domination.storm

object CLP {

  abstract trait Var {
    def +(v: Var): Var
    def -(v: Var): Var
    def and(v: Var): Var
    def isMorePreciseThan(v: Var): Boolean
    def fixed: Boolean
  }
  
  case class RangeVar(min: Int, max: Int) extends Var {
    require (min < max, "invalid range: min=" + min + ", max=" + max)
    
    val fixed = false
    
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
    
    override def and(variable: Var): Var = {
      variable match {
        case x @ Val(v) if (v >= min && v <= max) => x
        case RangeVar(min2, max2) => RangeVar(min max min2, max min max2)
        case ListVar(vals) => ListVar(vals filter (x => x >= min && x <= max))
        case _ => throw new CLPException(toString + " and " + variable + " is empty")
      }
    }
    
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

    val fixed = false
    
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
    
    override def and(variable: Var): Var = {
      variable match {
        case x @ Val(v) if vals.contains(v) => x
        case x @ RangeVar(_, _) => x and this 
        case ListVar(vals2) if (vals.containsSlice(vals2) || vals2.containsSlice(vals)) =>
          ListVar(vals.intersect(vals2))
        case _ => throw new CLPException(toString + " and " + variable + " is empty")
      }
    }
    
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

    val fixed = true

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

    override def and(variable: Var): Var = {
      if (this == variable)
        this
      else
        throw new CLPException(toString + " and " + variable + " is empty")
    }

    override def isMorePreciseThan(variable: Var): Boolean = {
      variable match {
        case Val(_) => false
        case _ => true
      }
    }
    override def toString = "Val(" + v + ")"
  }

  
  class Variable(initialVar: Var) {
    private var hist: List[Var] = initialVar :: Nil
    def current = hist.head
    def set(v: Var): Boolean = {
      if (v != current) {
	    hist = v :: hist
	    true
      }
      else false
    }
    def history = hist
    override def toString = "Variable(" + hist + ")"
  }
  
  class VariableNode(initialVar: Var) extends Variable(initialVar) {
    var incoming: List[Edge] = Nil
    var outgoing: List[Edge] = Nil
    
    override def set(v: Var): Boolean = {
      if (super.set(v)) {
        redistribute
	    true
      }
      else false
    }
    
    def redistribute: Unit = {
      //forward propagation
	  outgoing foreach (_.sourceChanged)
	  
	  //back propagation
	  val (fixed, notFixed) = incoming.partition(_.current.fixed)
	  val fixedSum = fixed.size match {
	    case 0 => None
	    case _ => Some(fixed.map(_.current).reduce(_ + _))
	  }
	  if (notFixed.size > 0) {
	    if (notFixed.size == 1) {
	      val candidate = fixedSum match {
	        case Some(x) => current - x
	        case None => current
	      }
	      if (candidate.isMorePreciseThan(notFixed.head.current))
	        notFixed.head.backPropagate(candidate)
	    } else {
	      //try to improve approximation
	      notFixed.foreach{
	        edge =>
	          val notFixedSum = notFixed.filter(_ != edge).map(_.current).reduce(_ + _)
	          val candidate = edge.current and (current - (fixedSum match {
		        case Some(x) => notFixedSum + x
		        case None => notFixedSum
	          }))
	          if (candidate.isMorePreciseThan(edge.current))
	            edge.backPropagate(candidate)
	      }
	      
	      
	    }
	  }
    }
    
    def calculateCurrent: Var = {
      val currentIncomings = incoming map (_.current)
      currentIncomings.reduce(_ + _)
    }
    
    override def toString = "VariableNode(history=" + history +
    		", incoming=" + incoming + ", outgoing=" + outgoing +")"
  }

  abstract class Edge(val source: VariableNode, val target: VariableNode) {
    def calculateCurrent: Var
    
    val state = new Variable(calculateCurrent) 
    def current = state.current
    
    def sourceChanged = {
      if (!current.fixed) {
        val newCurrent = calculateCurrent
        if (newCurrent.isMorePreciseThan(current)) {
          state.set(newCurrent)
          val changedTarget = target.calculateCurrent
          if (!target.current.fixed &&
            changedTarget.isMorePreciseThan(target.current))
            target.set(changedTarget)
        }
      }
    }
    
    def backPropagate(v: Var): Unit
    
  }
  
  class CLPException(msg: String) extends RuntimeException(msg)

}