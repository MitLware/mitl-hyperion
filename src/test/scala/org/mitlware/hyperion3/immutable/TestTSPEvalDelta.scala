package org.mitlware.hyperion3.immutable

import cats.data.State
import cats.implicits._
import monocle.Lens

import org.mitlware.hyperion3.immutable._
import org.mitlware.hyperion3.immutable.perturb._
import org.mitlware.hyperion3.immutable.accept._
import org.mitlware.hyperion3.immutable.isfinished._		

import org.mitlware.support.lang.BadFormatException
import org.mitlware.support.lang.Diag
import org.mitlware.support.lang.UnsupportedFormatException
import org.mitlware.support.math.Vec2

import org.mitlware.solution.permutation.ArrayForm
import org.mitlware.problem.tsp.TSP

import org.mitlware.hyperion3.immutable.perturb.permutation.EvaluateDelta._

import TSPHeuristics._

import org.junit.Test

import java.io._
import java.nio.charset.Charset
import java.nio._
import java.nio.file._

import org.junit.Assert._ 

///////////////////////////////////

class TestTSPDeltaEval {

  @Test
  def test: Unit = {
    
		val seed = 0xDEADBEEF
	  val maxIter = 10000	  
	  
    ///////////////////////////////
		
		// val path = System.getProperty( "user.dir" ) + "/resources/" + "unitTest.tsp"
	  val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

	  // val OptimalTourLength = 7467 // for unitTest.tsp    
    val OptimalTourLength = 27603 // for wi29.tsp
    
	  def relativeError(x: ArrayForm): Double = {
	    require( x.size() == tsp.numCities() )
	    (TSP.tourLength(x,tsp.getDistanceFn()) - OptimalTourLength)/OptimalTourLength
    }

    ///////////////////////////////
    
    case class MyEnv(iter: Iter, maxIter: MaxIter, rng: RNG, 
      fitness: Evaluate[MyEnv,ArrayFormDelta[MyEnv,Double],Double])

    type MyArrayFormDelta = ArrayFormDelta[MyEnv,Double] 
      
		def fitnessImpl(problem: TSP.TSPLibInstance)(x: ArrayForm): Double =
		  TSP.tourLength(x,problem.getDistanceFn())
		
    case class TourLengthEval[Env](problem: TSP.TSPLibInstance) extends Evaluate[Env,ArrayForm,Double] {
      override def apply(x: ArrayForm): State[Env,Double] = State[Env,Double] { env =>
        (env,fitnessImpl(problem)(x))
      }
    }
    
    case class TourLengthDeltaEval[Env](problem: TSP.TSPLibInstance) extends EvaluateDelta[Env,ArrayForm,Double] {
      override def apply(x: ArrayFormDelta[Env,Double]): State[Env,Double] = State[Env,Double] { env =>
        val fitnessByDeltaEval = x.updatedValue
        assert( fitnessByDeltaEval == fitnessImpl(problem)( x.updatedSol ) )        
        (env,fitnessByDeltaEval)
      }      
    }
		
  	val iterLens: Lens[MyEnv, Iter] = monocle.macros.GenLens[MyEnv] { _.iter }
	  val maxIterLens: Lens[MyEnv, MaxIter] = monocle.macros.GenLens[MyEnv] { _.maxIter }
	  val rngLens: Lens[MyEnv, RNG] = monocle.macros.GenLens[MyEnv] { _.rng }	  
	  val fitnessLens: Lens[MyEnv, Evaluate[MyEnv,MyArrayFormDelta,Double]] = monocle.macros.GenLens[MyEnv] { _.fitness }
    
    val perturb: Perturb[MyEnv,MyArrayFormDelta] = 
       RandomSwap( rngLens, (index1, index2 ) => RandomSwapDeltaTSP(tsp, index1, index2))
      
    val accept: Accept[MyEnv,MyArrayFormDelta] = AcceptImprovingOrEqual(isMinimizing=true, scala.math.Ordering.Double, fitnessLens)
    val isFinished: Condition[MyEnv,MyArrayFormDelta] = IterGreaterThanMaxIter(iterLens,maxIterLens)
		
	  val search = IteratedPerturbation(iterLens,perturb,accept,isFinished)
	  val fitness = TourLengthEval[MyEnv](tsp)
    val fitnessDelta = TourLengthDeltaEval[MyEnv](tsp)	  
	  
	  val initialS = ArrayFormDelta(new ArrayForm(tsp.numCities),fitness)
	  val initialEnv = MyEnv(Iter(0),MaxIter(maxIter),KnuthLCG64(seed), fitnessDelta )
	  
	  val searchS = for {
	    initial <- initialS;
	    result <- search( initial )
	  } yield { result }
	  
	  val (finalEnv,solution) = searchS.run( initialEnv ).value

	  Diag.println( relativeError(solution.updatedSol) )	  
	  Diag.println( fitnessImpl(tsp)(solution.updatedSol) )	  
	  
	  assertEquals( OptimalTourLength, TSP.tourLength(solution.updatedSol,tsp.getDistanceFn()), 0.0 )
  }
}

// End ///////////////////////////////////////////////////////////////
