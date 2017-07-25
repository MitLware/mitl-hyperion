package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

import org.mitlware.Diag

import org.mitlware.hyperion3.immutable._
import org.mitlware.hyperion3.immutable.perturb._
import org.mitlware.hyperion3.immutable.accept._
import org.mitlware.hyperion3.immutable.isfinished._		

import org.mitlware.support.lang.BadFormatException

import org.mitlware.support.lang.UnsupportedFormatException
import org.mitlware.support.math.Vec2

import org.mitlware.solution.permutation.ArrayForm
import org.mitlware.problem.tsp.TSP

import org.junit.Test

import java.io._
import java.nio.charset.Charset
import java.nio._
import java.nio.file._

import org.junit.Assert._ 

import org.mitlware.hyperion3.immutable.perturb.permutation.EvaluateFull

import TSPHeuristics._

///////////////////////////////////

class TestTSPHeuristics {

  @Test
  def testInsert: Unit = {

    val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

    ///////////////////////////////		

		val Seed = 0xDEADBEEF
    val rng = new scala.util.Random(Seed)
    
    val before = new ArrayForm( tsp.numCities, rng.self )
    
    for ( i <- 0 until tsp.numCities; j <- 0 until tsp.numCities  ) {

      val beforeValue = TSP.tourLength(before,tsp.getDistanceFn())      
      val after = EvaluateFull.RandomReinsert.perturb(before,i,j)
      val afterValue = TSP.tourLength(after,tsp.getDistanceFn())
      val afterValueViaDelta = beforeValue + RandomReinsertDeltaTSP.deltaEval(tsp)(before, i, j)
      assertEquals( afterValue, afterValueViaDelta, 0.0 )
    }
  }
  
  /////////////////////////////////
  
  @Test
  def testSwap: Unit = {

    val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

    ///////////////////////////////		

		val Seed = 0xDEADBEEF
    val rng = new scala.util.Random(Seed)
    
    val before = new ArrayForm( tsp.numCities, rng.self )
    
    for ( i <- 0 until tsp.numCities; j <- 0 until tsp.numCities  ) {
      
      val beforeValue = TSP.tourLength(before,tsp.getDistanceFn())      
      val after = RandomSwapDeltaTSP.perturb(before, i, j)  
      val afterValue = TSP.tourLength(after,tsp.getDistanceFn())
      val afterValueViaDelta = beforeValue + RandomSwapDeltaTSP.deltaEval(tsp)(before, i, j)      
        
      assertEquals( afterValue, afterValueViaDelta, 0.0 )
    }
  }
  
  /////////////////////////////////
  
//  def distanceMatrix(problem: TSP.TSPLibInstance): String = {
//    
//    val xx = for( i <- 0 until problem.numCities ) yield {
//      for( j <- 0 until problem.numCities ) yield TSPOps.distanceFn(problem)(i,j).toInt
//    }
//    xx.map { _.mkString(" [",",","]") }.mkString("[\n",",\n","\n]")
//  }
  
  @Test
  def testFlip: Unit = {

    val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

    ///////////////////////////////		

		val Seed = 0xDEADBEEF
    val rng = new scala.util.Random(Seed)
    
    val before = new ArrayForm( tsp.numCities, rng.self )
    
    for ( i <- 0 until tsp.numCities; j <- 0 until tsp.numCities  ) {
      
      val beforeValue = TSP.tourLength(before,tsp.getDistanceFn())      
      val after = EvaluateFull.RandomFlip.perturb(before, i, j)  
      val afterValue = TSP.tourLength(after,tsp.getDistanceFn())
      val afterValueViaDelta = beforeValue + RandomFlipDeltaTSP.deltaEval(tsp)(Perm(before), i, j)
      
      assertEquals( afterValue, afterValueViaDelta, 0.0 )
    }
  }

  /////////////////////////////////
  
  @Test
  def testTwoOptFirstImprovement: Unit = {

    val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
    // val path = System.getProperty( "user.dir" ) + "/resources/" + "sw24978.tsp"
    // val path = System.getProperty( "user.dir" ) + "/resources/" + "ca4663.tsp"
    // val path = System.getProperty( "user.dir" ) + "/resources/" + "rw1621.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

		val Seed = 0xDEADBEEF
    val rng = new scala.util.Random(Seed)
    
    val OptimalTourLength = 27603 // for wi29.tsp
    // val OptimalTourLength = 855597 // for sw24978.tsp
    // val OptimalTourLength = 1290319 // for ca4663.tsp
    // val OptimalTourLength = 26051 // for rw1621.tsp
    
    ///////////////////////////////
    
	  def relativeError(x: ArrayForm): Double = {
	    require( x.size() == tsp.numCities() )
	    (TSP.tourLength(x,tsp.getDistanceFn()) - OptimalTourLength)/OptimalTourLength
    }
    
    print( "initializing nearest cities..." )
    val startTime = System.currentTimeMillis()
    val twoOpt = TSPHeuristics.TwoOptFirstImprovement(tsp)
    val endTime = System.currentTimeMillis()
    println( s" (elapsed: ${(endTime-startTime)/1000.0})" )
      
    Diag.println( "running tests..." )
    
    val relativeErrors = for ( tests <- 0 until 10 ) yield {
      val initial = TSPHeuristics.nearestNeighbour(tsp, 1 + rng.nextInt(tsp.numCities) )

      val startTime = System.currentTimeMillis()
      
      val solution = twoOpt( initial, maxIter = 100 )

      val endTime = System.currentTimeMillis()
      
      Diag.println( s"elapsed: ${(endTime-startTime)/1000.0}" )

      val Threshold = 0.15
      val re = relativeError(solution)
      assertTrue( s"expected value <= $Threshold, found $re", re <= Threshold )
      
      re
    }
    
    val mean = relativeErrors.sum / relativeErrors.length.toDouble 
    val min = relativeErrors.min
    val max = relativeErrors.max    
    Diag.println( s"R.E.s: min: ${min}, max: ${max}, mean: ${mean}" )
  }
  
  /////////////////////////////////
  
  @Test
  def testTwoOptBestImprovement: Unit = {

    val path = System.getProperty( "user.dir" ) + "/resources/" + "wi29.tsp"
		val is = new FileInputStream( path )
		val tsp = new TSP.TSPLibInstance( is )

		val Seed = 0xDEADBEEF
    val rng = new scala.util.Random(Seed)
    
    val OptimalTourLength = 27603 
    
    ///////////////////////////////
    
	  def relativeError(x: ArrayForm): Double = {
	    require( x.size() == tsp.numCities() )
	    (TSP.tourLength(x,tsp.getDistanceFn()) - OptimalTourLength)/OptimalTourLength
    }
    
    val twoOpt = TSPHeuristics.TwoOptBestImprovement(tsp)

    val relativeErrors = for ( tests <- 0 until 1000 ) yield {
      val initial = TSPHeuristics.nearestNeighbour(tsp, 1 + rng.nextInt(tsp.numCities) )
      val solution = twoOpt( initial, maxIter = 100 )
      val Threshold = 0.2
      val re = relativeError(solution)
      assertTrue( s"expected value <= $Threshold, found $re", re <= Threshold )
      re
    }
    
    val mean = relativeErrors.sum / relativeErrors.length.toDouble 
    val min = relativeErrors.min
    val max = relativeErrors.max    
    Diag.println( s"R.E.s: min: ${min}, max: ${max}, mean: ${mean}" )
  }  
}

// End ///////////////////////////////////////////////////////////////
