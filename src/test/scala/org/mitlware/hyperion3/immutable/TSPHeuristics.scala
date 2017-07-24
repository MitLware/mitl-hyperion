package org.mitlware.hyperion3.immutable

import org.mitlware.problem.tsp.TSP
import org.mitlware.solution.permutation.ArrayForm

import org.mitlware.hyperion3.immutable.perturb.permutation._
import org.mitlware.hyperion3.immutable.perturb.permutation.EvaluateFull

///////////////////////////////////

// Perm: temporary helper class for ArrayForm <=> Array[Int] interop    
sealed trait Perm {
  def size: Int      
  def apply(i: Int): Int
  // require( i >= 0 && i < size )
  // ensuring( result >= 0 && result < size )    
}
    
object Perm {

  def isPerm(x: Array[Int]) = x.toSet.size == x.length &&
    x.forall { i => i >=0 && i < x.length }
  
  private final case class APerm(impl: Array[Int]) extends Perm {
    require( isPerm(impl) )
    
    override def size: Int = impl.length      
    override def apply(i: Int): Int = impl(i)
  }
  
  private final case class AFPerm(impl: ArrayForm) extends Perm {
    override def size: Int = impl.size
    override def apply(i: Int): Int = impl.get(i)
  }
    
  def apply(x: Array[Int]): Perm = new APerm(x)
  def apply(x: ArrayForm): Perm = new AFPerm(x)    
}

///////////////////////////////////
  
case class RandomSwapDeltaTSP(problem: TSP.TSPLibInstance, index1: Int, index2: Int) extends Delta[ArrayForm,Double] {
  override def apply(base: ArrayForm): (ArrayForm,Double) = {
    val deltaChanges = RandomSwapDeltaTSP.deltaEval(problem)(base, index1, index2)
    (RandomSwapDeltaTSP.perturb(base, index1, index2),deltaChanges)
  }
}

object RandomSwapDeltaTSP {

  def perturb(tour: ArrayForm, index1: Int, index2: Int): ArrayForm = {
    val array = (0 until tour.size()).toArray.map { i => 
      if( i == index1 ) 
        tour.get( index2 ) 
      else { 
        if( i == index2 ) tour.get( index1 ) else tour.get( i )          
      } 
    }
    new ArrayForm(array:_*)
  }

  /////////////////////////////////

  def deltaEval(problem: TSP.TSPLibInstance)(tour: ArrayForm, index1: Int, index2: Int): Double = {
    require( problem.isSymmetric() )
	  
    if( index1 == index2 ) {
      0
    } 
    else {      
    
      var dist = 0.0
      var pos1 = index1
      var pos2 = index2
    
      def distance(i: Int, j: Int): Double = {
        val city1 = tour.get(i); val city2 = tour.get(j)
        problem.getDistanceFn()(city1+1, city2+1)      
      }
    
      val len = problem.numCities
		
      //   @see https://www.coin-or.org/Ots/docs/tutorial/simple/objfunc2.html    
      // Logic below requires pos1 < pos2
      if( pos1 > pos2 ) {
        val temp = pos2
        pos2 = pos1
        pos1 = temp
      }
      assert( pos1 < pos2 )
    
	    val prev1 = ( pos1 - 1 + len ) % len
  	  val prev2 = ( pos2 - 1 + len ) % len
  	  val next1 = ( pos1 + 1 ) % len
      val next2 = ( pos2 + 1 ) % len
	  
      // Prior objective value
      // double dist = solution.getObjectiveValue()[0];
            
      // Treat a pair swap move differently
      if( pos1 + 1 == pos2 ) {
     
        // A-B-C-D-E: swap C and D, say (works for symmetric matrix only)
        // dist -= distance( matrix[ tour[pos1-1] ][ tour[pos1] ];           // -BC
        dist -= distance( prev1, pos1 )// -BC      
        dist -= distance( pos2,next2)   // -DE
        dist += distance( prev1,pos2)           // +BD
        dist += distance( pos1, next2)   // +CE
        dist
      }
      else {
        if( pos1 == 0 && pos2 == len - 1 ) {
          
          // pair swap special case: 
          // A-B-C-D-E: swap A and E (works for symmetric matrix only)
          dist -= distance( prev2, pos2 ) // -DE      
          dist -= distance( pos1,next1)   // -AB
          dist += distance( prev2,pos1)   // +DA 
          dist += distance( pos2, next1)  // +EB
          dist
        }
        else {
          
          // Else the swap is separated by at least one city
          // A-B-C-D-E-F: swap B and E, say
          dist -= distance( prev1,pos1 )    // -AB
          dist -= distance( pos1, next1 )   // -BC
          dist -= distance( prev2,pos2 )    // -DE
          dist -= distance( pos2, next2 )   // -EF
            
          dist += distance( prev1,pos2 )    // +AE
          dist += distance( pos2,next1 )    // +EC
          dist += distance( prev2,pos1 )    // +DB
          dist += distance( pos1, next2 )   // +BF
          dist      
        }
      }
    }
  }
}

///////////////////////////////////
  
case class RandomReinsertDeltaTSP(problem: TSP.TSPLibInstance, index1: Int, index2: Int) extends Delta[ArrayForm,Double] {
  override def apply(base: ArrayForm): (ArrayForm,Double) = {
    val deltaChanges = RandomReinsertDeltaTSP.deltaEval(problem)(base, index1, index2)
    (EvaluateFull.RandomReinsert.perturb(base, index1, index2),deltaChanges)
  }
}
  
object RandomReinsertDeltaTSP {
    
  def deltaEval(problem: TSP.TSPLibInstance)(tour: ArrayForm, i1: Int, i2: Int): Double = {
      
    if( i1 == i2 ) 
      0.0 
    else {
      
      val hi = math.max(i1,i2)
      val lo = math.min(i1,i2)
	    
	    if( lo == 0 && hi == problem.numCities - 1 )
	      0.0
      else {
		    val cityHi = tour.get(hi)
		    val cityLo = tour.get(lo)		

		    val prevLo = if(lo == 0) tour.get(problem.numCities - 1) else tour.get(lo - 1)
		
		    val prevHi = if(hi == 0)	tour.get(problem.numCities - 1) else tour.get(hi - 1)
		    val nextHi = if(hi == problem.numCities - 1) tour.get(0) else tour.get(hi + 1)

		    val distance = TSPHeuristics.distanceFn(problem)
		
		    val oldCost = distance(prevHi, cityHi) + distance(cityHi, nextHi) + distance(prevLo, cityLo)
		    val newCost = distance(prevLo, cityHi) + distance(cityHi, cityLo) + distance(prevHi, nextHi)
		      newCost - oldCost
      }
    }	    
  }
}
  
///////////////////////////////////

case class RandomFlipDeltaTSP(problem: TSP.TSPLibInstance, index1: Int, index2: Int) extends Delta[ArrayForm,Double] {
  override def apply(base: ArrayForm): (ArrayForm,Double) = {
    val deltaChanges = RandomFlipDeltaTSP.deltaEval(problem)(Perm(base), index1, index2)
    (EvaluateFull.RandomFlip.perturb(base, index1, index2),deltaChanges)
  }
}
  
object RandomFlipDeltaTSP {
    
  def deltaEval(problem: TSP.TSPLibInstance)(tour: Perm, index1: Int, index2: Int): Double = {

    val city1 = tour(index1)
    val city2 = tour(index2)		
    val prev1 = if(index1 == 0)	tour(problem.numCities - 1) else tour(index1 - 1)	
    val next2 = if(index2 == problem.numCities - 1)	tour(0) else tour(index2 + 1)

    val distance = TSPHeuristics.distanceFn(problem)
    
    if(prev1 == city2 || next2 == city1) 
      0
    else {
      val currentCost = distance(city1, prev1) + distance(city2, next2)		
      val newCost = distance(city1, next2) + distance(city2, prev1)
      newCost - currentCost
    }
  }
}

///////////////////////////////////
  
object TSPHeuristics {
  
  type Tour = ArrayForm

  def distanceFn(problem: TSP.TSPLibInstance) = 
    (city1: Int, city2: Int) => problem.getDistanceFn()(city1+1, city2+1)
  
  /////////////////////////////////
  
  def nearestNeighbour(tsp: TSP.TSPLibInstance, startCity: Int): ArrayForm = {
    require( startCity >= 1 && startCity <= tsp.numCities() )
    var unvisited = (1 to tsp.numCities() ).toSet - startCity

		var tour = List(startCity)
		while( tour.length < tsp.numCities() ) {
		  val nearest = unvisited.minBy { v => tsp.getDistanceFn()(tour.last, v) }
		  unvisited = unvisited - nearest
		  tour = tour :+ nearest 
		}
		
		new ArrayForm( tour.map { _ - 1 }.toArray:_* )
	}

  def bestNearestNeighbour(tsp: TSP.TSPLibInstance): ArrayForm = {
		var bestStart = (1 to tsp.numCities() ).minBy { startCity => 
      TSP.tourLength(nearestNeighbour(tsp, startCity), tsp.getDistanceFn() ) 
    }
		nearestNeighbour(tsp, bestStart)
  }

  /////////////////////////////////

	// nearestCities(i)(j) is the i-th nearest city to j
	def nearestCities(problem: TSP.TSPLibInstance, retainNumNearest: Int): Map[Int,List[Int]] = {
	  require( problem.isSymmetric() )
	  require( retainNumNearest >= 0 && retainNumNearest < problem.numCities )
    
	  val distance = distanceFn(problem)

	  val elems = for( i <- 0 until problem.numCities ) yield {
	    val indices = (0 until problem.numCities).toSet - i
	    val sortedByDistance = indices.toList.sortBy { j => distance( i,j ) }
	    i -> sortedByDistance.take( retainNumNearest )
	  }
	  Map() ++ elems
	}
  
	/////////////////////////////////
	
  case class TwoOptFirstImprovement(problem: TSP.TSPLibInstance) {
    
    private val NumNearest = 8 // As used by HyFlex
    val nearestCity = nearestCities(problem,NumNearest)
    
	  def apply(ptour: ArrayForm, maxIter: Int): ArrayForm = {

		  var tour = ptour.toArray
		  var iter = 0
		  var improvement = true		
		  while( improvement && iter < maxIter ) {
			  improvement = false
			  for( i <- 0 until problem.numCities; j <- 0 until NumNearest ) {
          if( RandomFlipDeltaTSP.deltaEval(problem)(Perm(tour), i, nearestCity(i)(j) ) < 0.0 ) {
			      tour = EvaluateFull.RandomFlip.impl(tour, i, nearestCity(i)(j) )					  
            improvement = true
          }
        }
			  iter += 1
		  }
		  new ArrayForm(tour:_*)
	  }
  }
  
  /////////////////////////////////

  case class TwoOptBestImprovement(problem: TSP.TSPLibInstance) {
    
    private val NumNearest = 8 // As used by HyFlex
    val nearestCity = nearestCities(problem,NumNearest)
    
	  def apply(ptour: ArrayForm, maxIter: Int): ArrayForm = {

		  var tour = ptour.toArray
		  var iter = 0
		  var improvement = true		
		  while( improvement && iter < maxIter ) {
			  improvement = false
			  for( i <-0 until problem.numCities ) {
			    
			    val (j,cost) = ( 0 until NumNearest ).map { j => 
			      (j,RandomFlipDeltaTSP.deltaEval(problem)(Perm(tour), i, nearestCity(i)(j) ))
			    }.minBy { _._2 }

			    if( cost < 0 ) {
            tour = EvaluateFull.RandomFlip.impl(tour, i, nearestCity(i)(j) )					  
					  improvement = true;
			    }
        }
			  iter += 1
		  }
		  new ArrayForm(tour:_*)
	  }
  }

	/////////////////////////////////
  
  /***************************************
  
	private def insert(initialTour: Array[Int], x: Int, index: Int): Array[Int] = {
		val newTour = new Array[Int]( initialTour.length + 1 )
		System.arraycopy(initialTour, 0, newTour, 0, index)
		newTour(index) = x
		if(index < initialTour.length)
			System.arraycopy(initialTour, index, newTour, index + 1, initialTour.length - index)
			
		newTour
	}

	def insertionCost(problem: TSP.TSPLibInstance)(tour: Tour, city1: Int, index: Int): Double = {
	  
		val distance = distanceFn(problem)    
  
		if( index == 0 || index == problem.numCities )
			distance(city1, tour.get(0)) + distance(city1, tour.get(problem.numCities - 1))
		else
		  distance(city1, tour.get(index - 1)) + distance(city1, tour.get(index))
	}		
	
  private def greedyInsertionImpl(problem: TSP.TSPLibInstance)(ptour: Array[Int], toInsert: Array[Int], pcost: Double): Array[Int] = {
    
    val distance = distanceFn(problem)
		  
    def insertionCostArray(tour: Array[Int], city1: Int, index: Int): Double =
		  if( index == 0 || index == problem.numCities )
			  distance(city1, tour(0)) + distance(city1, tour(problem.numCities - 1))
		  else
		    distance(city1, tour(index - 1)) + distance(city1, tour(index))
    
		var min = Double.MaxValue
		var index = 0
		var cost = pcost
		var tour = ptour
		for( i <- 0 until toInsert.length) {
      min = insertionCostArray(tour, toInsert(i), 0)
			index = 0;
			for( j <- 1 to tour.length ) {
				val temp = insertionCostArray(tour, toInsert(i), j)
				if( temp < min ) {
					min = temp
					index = j
				}	
			}	
			cost += min
      tour = insert(tour, toInsert(i), index)
		}
		tour
	}
	
	def greedyInsertion(problem: TSP.TSPLibInstance)(cost: Double): Array[Int] = {
		val tour = Array(0, 1)
		val toInsert = new Array[Int](problem.numCities - 2)
		for( i <- 2 until problem.numCities )
			toInsert(i-2) = i
		
    val distance = distanceFn(problem)
		greedyInsertionImpl(problem)(tour, toInsert, cost + 2 * distance(0, 1))
  }	
	
  ***************************************/
}

// End ///////////////////////////////////////////////////////////////
