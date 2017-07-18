package org.mitlware.hyperion3.immutable.perturb

import cats.data.State
import monocle.Lens

import org.mitlware.hyperion3.immutable._
import org.mitlware.solution.permutation.ArrayForm

///////////////////////////////////

object Permutation {
  
///////////////////////////////////
  
/******  
case class RandomShuffle[Env]() extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomShuffle( RNG.self )
    (env,result)
  } 
}
******/
  
///////////////////////////////////

	/**
	 * Swap two randomly-chosen elements. 
	 * @book{Eiben:2003:IEC:954563,
	 * 	 author = {Eiben, Agoston E. and Smith, J. E.},
	 * 	 title = {Introduction to Evolutionary Computing},
	 * year = {2003},
	 * isbn = {3540401849},
	 * publisher = {SpringerVerlag},
	 * }
	 */
	
case class RandomSwap[Env](rngLens: Lens[Env,RNG]) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = for {
      index1 <- RNG.nextInt(rngLens, x.size());
      index2 <- RNG.nextInt(rngLens, x.size())      
    } yield {
      val result = x.toArray()      
      val temp = result(index1)
      result(index1) = result(index2) 
      result(index2) = temp
      new ArrayForm(result:_*) 
  }
}

///////////////////////////////////

/********

case class RandomInsert[Env]() extends Perturb[Env,ArrayForm] {

  private def randomInsert[Env] = (x: ArrayForm) => State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomInsert( RNG.self )
    (env,result)
  }
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = randomInsert[Env](x)  
}

///////////////////////////////////

case class RandomShuffleSubset[Env](mutationDegree: MutationStrength) extends Perturb[Env,ArrayForm] {
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomShuffleSubset( new org.mitlware.support.math.UnitInterval(mutationDegree.asDouble), RNG.self )
    (env,result)
  }  
}

///////////////////////////////////

case class ReverseSubtours[Env](mutationDegree: MutationStrength) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.nOpt(new org.mitlware.support.math.UnitInterval(mutationDegree.asDouble), RNG.self )
    (env,result)
  }  
}
********/

/*************************

case class BestImproving2Opt[Env](tourLengthLens: monocle.Lens[Env, Evaluate[Env,ArrayForm,Double]],maxPasses: Int) 
  extends Perturb[Env,ArrayForm] {

  private def reverseSubtour(x: ArrayForm, i: Int, j: Int): ArrayForm = {
    var upper = j
		while (upper < i) {
			upper += x.size()
		}
		
    val a = x.toArray
		for (k <- 0 until (upper - i + 1) / 2 ) {
			val temp =a(i+k)
			a(i+k ) = a(upper-k)
			a(upper-k) = temp
		}
    new ArrayForm(a:_*)
	}

//  private def reverseSubtour(x: ArrayForm, i: Int, j: Int): ArrayForm = {
//    val l = x.toArray().toList
//    l.take(i)
//    l.drop(i).dropRight(x.size() - j).reverse
//    l.takeRight(j)
//    ???
//  }

  // Adapted from https://github.com/dhadka/TSPLIB4J/blob/master/src/org/moeaframework/problem/tsplib
  
  override def apply(tour: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>  
 
    def evalHackFIXME(x: ArrayForm): Double = 
      tourLengthLens.get( env ).apply(x).runA(env).value
    
		// tours with 3 or fewer nodes are already optimal
		if (tour.size() < 4) {
			(env,tour)		
    }
		else {
// jeep.lang.Diag.println( "entering Best 2-opt" )		
		  var incumbent = tour
		  var incumbentValue = evalHackFIXME( incumbent )
		  var modified = true
		  var numPasses = 0
		  
		  while( modified && numPasses < maxPasses ) {
			  modified = false
			  for( i <- 0 until tour.size; j <- i+2 until tour.size ) {
			    
          val incoming = reverseSubtour(incumbent, i+1, j)
          val incomingValue = evalHackFIXME( incoming )
          if( incomingValue < incumbentValue ) {
			  			modified = true            
              incumbentValue = incomingValue
              incumbent = incoming
          }
        }
        numPasses += 1			  
      }
// jeep.lang.Diag.println( "leaving Best 2-opt" )		  
			(env,incumbent)		  
    }
  }
}

*************************/

///////////////////////////////////

} // object Permutation {

// End ///////////////////////////////////////////////////////////////
