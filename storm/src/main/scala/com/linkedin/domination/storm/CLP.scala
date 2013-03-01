package com.linkedin.domination.storm

import scala.annotation.tailrec

object CLP {
  
  //TODO implement greater than, result is [0, 1] - probability
  
  val MaxVarListLength = 64
  val MaxPropagationDepth = 16
  val NonNegative = RangeVar(0, Int.MaxValue)
  val Positive = RangeVar(1, Int.MaxValue)
  val Zero = Val(0)
  
  val MaxPopulation: Int = 60 * (40 + 10000 * 4)
 

  abstract trait Var {
    def +(v: Var): Var
    def -(v: Var): Var
    def op(oper: Int => Int): Var
    def *(d: Double): Var = op( x => (x.toDouble * d).toInt)
    def /(d: Double): Var = op( x => (x.toDouble / d).toInt)
    def divCeil(d: Double): Var = op( x => (Math.ceil(x.toDouble / d)).toInt)
    def and(v: Var): Var
    def or(v: Var): Var
    def intersects(v: Var): Boolean
    def isMorePreciseThan(v: Var): Boolean
    def fixed: Boolean
    def isPositive = intersects(Positive)
    def isNonNegative = intersects(NonNegative)
    def min: Int
    def max: Int
  }
  
  def possiblyInvalid(v: => Var): Var = {
    try {
      v
    } catch {
      case _ => Zero
    }
  }
  
  def listVarOrVal(list: List[Int]) = {
    val l = list.filter(x => x >= 0 && x < MaxPopulation)
    if (l.size == 1)
      Val(l.head)
    else
      restrictLength(l)
  }
    
  
  def restrictLength(vals: List[Int]): Var =
    if (vals.size > MaxVarListLength)
      RangeVar(vals.head, vals.last)
    else
      ListVar(vals)
  
  case class Val(v: Int) extends Var {
    require(v >= 0)

    val fixed = true
    
    override def min = v
    
    override def max = v
    
    override def op(oper: Int => Int) = Val(oper(v))
    
    override def intersects(variable: Var): Boolean = {
      variable match {
        case Val(v2) => v == v2
        case RangeVar(min, max) => v >= min && v <= max
        case ListVar(vals) => vals.contains(v)
      }
    }
    
    override def +(variable: Var): Var = {
      variable match {
        case Val(v2) => Val(v + v2)
        case RangeVar(min, max) => rangeVarOrVal(v + min, v + max)
        case ListVar(vals) => listVarOrVal(vals map (_ + v))
      }
    }

    override def -(variable: Var): Var = {
      variable match {
        case Val(v2) => Val(v - v2)
        case RangeVar(min, max) => rangeVarOrVal(v - max, v - min)
        case ListVar(vals) => listVarOrVal((vals map (v - _)).reverse.filter(_ >= 0))
      }
    }

    override def and(variable: Var): Var = {
      variable match {
        case Val(x) if this == variable => this
        case RangeVar(_, _) => variable and this
        case ListVar(_) => variable and this
        case _ => throw new CLPException(toString + " and " + variable + " is empty") 
      }
    }

    override def or(variable: Var): Var = {
      variable match {
        case Val(v2) => if (v == v2) this else ListVar(List(v, v2).sorted)
        case RangeVar(min, max) => RangeVar(v min min, v max max)
        case ListVar(vals) =>
          listVarOrVal((v :: vals).distinct.sorted)
      }
    }

    override def isMorePreciseThan(variable: Var): Boolean = {
      variable match {
        case Val(_) => false
        case _ => true
      }
    }
    override def toString = "Val(" + v + ")"
  }

  def rangeVarOrVal(min: Int, max: Int): Var =
    if ((min max 0) == (max min MaxPopulation))
      Val(max)
    else
      RangeVar((min max 0), (max min MaxPopulation))
  
  case class RangeVar(min: Int, max: Int) extends Var {
    require (min < max, "invalid range: min=" + min + ", max=" + max)
    
    val fixed = false

    override def intersects(variable: Var): Boolean = {
      variable match {
        case Val(_) => variable intersects this 
        case RangeVar(min2, max2) => !((max < min2) || (max2 < min))
        case ListVar(vals) => rangeVarOrVal(vals.head, vals.last) intersects this
      }
    }

    override def op(oper: Int => Int) = {
      val newMin = oper(min)
      val newMax = oper(max)
      rangeVarOrVal(newMin, newMax)
    }
    
    override def +(variable: Var): Var = {
      variable match {
        case Val(_) => variable + this
        case RangeVar(min2, max2) => rangeVarOrVal(min + min2, max + max2)
        case ListVar(vals) => rangeVarOrVal(min + vals.head, max + vals.last)
      }
    }
    
    override def -(variable: Var): Var = {
      variable match {
        case Val(v) => rangeVarOrVal(min - v, max - v)
        case RangeVar(min2, max2) => rangeVarOrVal(min - max2, max - min2)
        case ListVar(vals) => rangeVarOrVal(min - vals.last, max - vals.head)
      }
    }
    
    override def and(variable: Var): Var = {
      variable match {
        case x @ Val(v) if (v >= min && v <= max) => x
        case RangeVar(min2, max2) =>
          if ((min max min2) == (max min max2))
            Val(min max min2)
          else  
            RangeVar(min max min2, max min max2)
        case ListVar(vals) =>
          listVarOrVal(vals filter (x => x >= min && x <= max))
        case _ => throw new CLPException(toString + " and " + variable + " is empty")
      }
    }
    
    override def or(variable: Var): Var = {
      variable match {
        case Val(_) => variable or this 
        case RangeVar(min2, max2) => rangeVarOrVal(min min min2, max max max2)
        case ListVar(vals) => rangeVarOrVal(min min vals.head, max max vals.last)
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
  
  case class ListVar(vals: List[Int]) extends Var {
    //TODO remove expensive checks before submitting
    require (vals.head >= 0)
    require (vals == vals.sorted, "list must be sorted")
    require (vals == vals.distinct, "list can not contain repeated elements")
    require (vals.size > 1, "list must have at least two elements")

    val fixed = false
    
    override def min = vals.head
    
    override def max = vals.last
    
    override def intersects(variable: Var): Boolean = {
      variable match {
        case Val(_) => variable intersects this 
        case RangeVar(_, _) => variable intersects this
        case ListVar(vals2) => !vals.intersect(vals2).isEmpty
      }
    }

    override def op(oper: Int => Int) = {
      val prepared = vals.map(oper).distinct.sorted
      listVarOrVal(prepared)
    }
    
    override def +(variable: Var): Var = {
      variable match {
        case Val(_) => variable + this
        case RangeVar(_, _) => variable + this
        case ListVar(vals2) => {
            listVarOrVal(vals.foldLeft(List[Int]()) {
              (list, x) =>
                vals2.foldLeft(list) {
                  (l, y) => x + y :: l
                }
            }.distinct.sorted)
        }
      }
    }
    
    override def -(variable: Var): Var = {
      variable match {
        case Val(v) => listVarOrVal((vals.map(_ - v)).filter(_ >= 0))
        case RangeVar(min, max) => rangeVarOrVal(vals.head - max, vals.last - min)
        case ListVar(vals2) => {
            listVarOrVal(vals.foldLeft(List[Int]()) {
              (list, x) =>
                vals2.foldLeft(list) {
                  (l, y) => x - y :: l
                }
            }.distinct.sorted)
        }
      }
    }
    
    override def and(variable: Var): Var = {
      variable match {
        case x @ Val(v) if vals.contains(v) => x
        case x @ RangeVar(_, _) => x and this 
        case ListVar(vals2) => listVarOrVal(vals.intersect(vals2))
        case _ => throw new CLPException(toString + " and " + variable + " is empty")
      }
    }

    override def or(variable: Var): Var = {
      variable match {
        case Val(_) => variable or this
        case RangeVar(_,_) => variable or this
        case ListVar(vals2) => listVarOrVal((vals ::: vals2).distinct.sorted)
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

  class Node(initialVar: Var, mask: RangeVar) extends Variable(initialVar) {
    var incoming: List[Edge] = Nil
    var outgoing: List[Edge] = Nil
    
    def incomingChanged(depth: Int) = recalculate(incoming, depth)

    def outgoingChanged(depth: Int) = recalculate(outgoing, depth)
    
    def applyMask = {
      val candidate = withMask(current)
      if (candidate.isMorePreciseThan(current)) {
        set(candidate)
        propagateBothWays(MaxPropagationDepth)
      }
    }
    
    private def recalculate(edges: List[Edge], depth: Int) = {
      val sum = calculate(edges)
      if (sum.isMorePreciseThan(current)) {
        set(sum)
      }
      propagateBothWays(depth)
    }
    
    @tailrec
    final def propagateBothWays(depth: Int): Unit = {
      if ((depth > 0) &&
          (propagate(incoming, (e, v, d) => e.backPropagate(v, d), depth - 1) ||
           propagate(outgoing, (e, v, d) => e.propagate(v, d), depth - 1)))
      propagateBothWays(depth - 1)
    }
    
    private def propagate(edges: List[Edge], propagator: (Edge, Var, Int) => Unit, depth: Int): Boolean = {
      var improved = false
	  val (fixed, notFixed) = edges.partition(_.current.fixed)
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
	      if (candidate.isMorePreciseThan(notFixed.head.current)) {
	        improved = true
	        propagator(notFixed.head, candidate, depth)
	      }
	    } else {
	      //try to improve approximation
	      notFixed.foreach{
	        edge =>
	          val notFixedSum = notFixed.filter(_ != edge).map(_.current).reduce(_ + _)
	          val candidate = edge.current and (current - (fixedSum match {
		        case Some(x) => notFixedSum + x
		        case None => notFixedSum
	          }))
	          if (candidate.isMorePreciseThan(edge.current)) {
	            improved = true
	            propagator(edge, candidate, depth)
	          }
	      }
	    }
	  }
	  improved
    }

    def withMask(v: Var): Var =
      v match {
        case Val(x) if (x >= mask.min && x <= mask.max) => v
        case RangeVar(min, max) => if ((min max mask.min) == (max min mask.max))
          Val(max min mask.max)
        else
          RangeVar(min max mask.min, max min mask.max)
        case ListVar(list) => listVarOrVal(list.filter(x => x >= mask.min && x <= mask.max))
      }
    
    def calculate(edges: List[Edge]): Var = {
      val sum = edges.map(_.current).reduce(_ + _)
      withMask(sum)
    }
    
    override def toString = "VariableNode(history=" + history +
    		", incoming=" + incoming + ", outgoing=" + outgoing +")"
  }

  abstract class Edge(val src: Option[Node], var tgt: Option[Node]) {
    
    def calculateCurrent: Var
    
    lazy val state = new Variable(calculateCurrent) 
    def current = state.current
    
    def set(v: Var) = state.set(v)
    
    def propagate(v: Var, depth: Int) = {
      if (!current.fixed) {
        val newCurrent = calculateCurrent
        if (newCurrent.isMorePreciseThan(v))
          set(newCurrent)
        else
          set(v)

        tgt.foreach(_.incomingChanged(depth))
      }
    }
    
    def backPropagate(v: Var, depth: Int): Unit = {
      //TODO is it possible to narrow this?
      set(v)
      src.foreach(_.outgoingChanged(depth))
    }
    
  }
  
  class CLPException(msg: String) extends RuntimeException(msg)

}