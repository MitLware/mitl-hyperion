package org.mitlware.hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._

import hyperion3.benchmarks.onemax.mutable._

import hyperion3.mutable._

import org.mitlware.solution.bitvector._

import java.lang.{Double=>JDouble}
// ^ java.lang.Double is Comparable, scala.lang.Double isn't

import org.junit._
import org.junit.Assert._

import scala.util.Random

//////////////////////////////////////////////////////////////////////

class TestCombinatorialAssembly {

  implicit val workspace = Workspace()

  // Ordering.by { x => ??? }
  
  /////////////////////////////////
  
  def testCombinatorialAssemblyImpl(numBits: Int, mkVar: VariableFactory) : Unit = {

    val random = new scala.util.Random( 0 )

    /////////////////////////////
    
    val eval = Onemax.countOnes
    val prefer = eval
    
    val iterCount = mkVar.readWrite(0L)
    val startTime = mkVar.readWrite(0L)

    val initialState = new BitVector( numBits )

    val MaxIter = 16*numBits
    
    import hyperion3.mutable.isfinished._    
    val maxIterReached = maxIterations[BitVector](MaxIter,iterCount) _
    def optimumFound(x: BitVector) = eval(x) == 1.0
    def isFinished(x: BitVector) = optimumFound(x) || maxIterReached(x) 
    
    type Feature = Int // bit index
    // val TabuListLength = 7
    
    import org.apache.commons.collections15.buffer.CircularFifoBuffer

    def featureFn( incumbent: BitVector, incoming: BitVector ) : Set[Feature] = {
      require( incoming.length == incumbent.length )
      
      var result = Set.empty[Int]
      for( i <- 0 until incoming.length )
        if( incoming.get(i) != incumbent.get(i) )
          result += i

      result
    }
    
    import hyperion3.mutable.accept._
     
    
    val moves = Seq( Move.best( eval ), Move.bestImproving( eval ) )
    
    val perturbs = Seq( Onemax.uniformMutation( mkVar.readWrite(random) ),
      Perturb.compose( Onemax.bitflipLocality _, Move.best( eval ) ),
      Perturb.compose( Onemax.bitflipLocality _, Move.bestImproving( eval ) )
      // Perturb.compose( Onemax.bitflipLocality _, Move.firstImproving( eval ) )      
    ) 

    def perturbsFromMove(m: Move[BitVector]) = Seq( 
      Onemax.uniformMutation( mkVar.readWrite(random) ),
      Perturb.compose( Onemax.bitflipLocality _, m )
      // Perturb.compose( Onemax.bitflipLocality _, Move.firstImproving( eval ) )      
    ) 
    
    val tabuAccepts = for( i <- List.range( 1,8 ) ) yield {
      val tabuList = mkVar.readWrite( new CircularFifoBuffer[Set[Feature]](i) )
      Tabu.strict(/*prefer, */featureFn, tabuList) _
    }    
  
    val accepts: Seq[Accept[BitVector]] = Seq( Accept.improving(prefer), Accept.improvingOrEqual(prefer) )  ++ tabuAccepts.map( liftAccept )
    
//    def tabuAccept(tenure: Int): Accept[BitVector] = {
//      val tabuList = mkVar.readWrite( new CircularFifoBuffer[Set[Feature]](i) )
//      Tabu.strict(/*prefer, */featureFn, tabuList) _
//    }    
    import spire.random._
    import spire.random.rng._

    def tabuAccept(tenure: Dist[Int]): Dist[Accept[BitVector]] = {
      tenure.map { i =>
        val tabuList = mkVar.readWrite( new CircularFifoBuffer[Set[Feature]](i) )
        liftAccept(Tabu.strict(/*prefer, */featureFn, tabuList) _)
      }
    }    
    
    for( perturb <- perturbs; accept <- accepts ) {
      val search = LocalSearch.returnBest(perturb, accept, isFinished _, prefer, iterCount, startTime)
      val result = search(initialState)
      //println(workspace)
    }
    
   // import ds.DS
    
    val designExhaustive = for( move <- moves; perturb <- perturbs; accept <- accepts ) yield {
      LocalSearch.returnBest(perturb, accept, isFinished _, prefer, iterCount, startTime)
    }

    val rng = spire.random.rng.Cmwc5()
    
    val design = for( move <- Dist.oneOf(Move.best( eval ), Move.bestImproving( eval ));    
      acc <- Dist.oneOf(accepts:_*); 
      perturb <- Dist.oneOf(perturbs:_*)  ) yield {
      LocalSearch.returnBest(perturb, acc, isFinished _, prefer, iterCount, startTime)
    }
    
    val designConditional = for( 
      move <- Dist.oneOf(Move.best( eval ), Move.bestImproving( eval ));
      tabuTenure = Dist.uniform( 1, 8 ); 
      acc <- tabuAccept(tabuTenure); 
      perturb <- Dist.oneOf(perturbsFromMove(move):_*)  ) yield {
      LocalSearch.returnBest(perturb, acc, isFinished _, prefer, iterCount, startTime)
    }
  }
  
  /////////////////////////////////
  
  @Test
  def testCombinatorialAssembly() : Unit = {
  
    // val mkVar = hyperion3.mutable.GlobalVars.Factory
    // implicit val workspace = Workspace()
    val mkVar = new hyperion3.mutable.WorkspaceVars.Factory
    
    val NumBits = 64
    testCombinatorialAssemblyImpl(NumBits,mkVar)
  }
}

// End ///////////////////////////////////////////////////////////////
