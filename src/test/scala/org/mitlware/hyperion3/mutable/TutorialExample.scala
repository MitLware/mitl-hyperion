package org.mitlware.hyperion3.mutable

import org.mitlware.hyperion3.mutable._

// import hyperion3.benchmarks.onemax.mutable._

import org.mitlware.solution.bitvector._
import org.mitlware.problem.bitvector._
import org.mitlware.mutable._

import java.util.Random

object TutorialExample {
  
  def main(args: Array[String]): Unit = {
		
    val numBits = 16
    val random = new Random(0)
    
    val target = new BitVector( numBits, random )
    val eval = new BitVectorProblems.Mimicry(target)
    val prefer = Prefer.from(eval)
    val accept = Accept.improving(eval)

    val initialState = new BitVector(numBits, random)
    println( "source: " + initialState )			
    val perturb = new BitVectorProblems.UniformMutation( random )

    ///////////////////////////		

    // Using the GlobalVars.Factory is effectively the same as 
    // `traditional' ad hoc variable declaration
    // val varFactory = hyperion3.mutable.GlobalVars.Factory
    implicit val workspace = Workspace()
    implicit val varFactory = new org.mitlware.hyperion3.mutable.WorkspaceVars.Factory

    val iterCount = ReadWrite(0L)
    val startTime = ReadWrite(System.currentTimeMillis())
    
    val MaxIter = 1000
    val MaxTime = 10000
    val isFinished = (incumbent: BitVector) => incumbent.equals(target) || iterCount.get == MaxIter || System.currentTimeMillis() - startTime.get > MaxTime  
    
    val search = LocalSearch(perturb, accept, isFinished, Option.apply(prefer), iterCount,startTime)
    
    val result = search.apply(initialState);      
    println( "result: " + result + ", eval: " + eval.apply(result) );
    println( "Collected ad hoc variables: " + workspace );
  }
}

// End ///////////////////////////////////////////////////////////////
