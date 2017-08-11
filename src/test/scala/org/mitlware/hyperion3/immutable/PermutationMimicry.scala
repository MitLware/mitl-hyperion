package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._
import monocle.Lens

//import scalaz.State
//import scalaz.Lens
//import scalaz.Semigroup

import org.mitlware.Diag

import org.mitlware.hyperion3.immutable._
import org.mitlware.hyperion3.immutable.perturb.permutation.EvaluateDelta._

import org.mitlware.solution.permutation.ArrayForm
	  
///////////////////////////////////

object PermutationMimicry {
  
  /////////////////////////////////
  
  def search(source: ArrayForm, target: ArrayForm, numIter: Int, seed: Long): (ArrayForm,Int) = {

    // val rng = new scala.util.Random(0xDEADBEEF)
    val rng = new scala.util.Random(seed)
    
    case class MyEnv(iter: Iter, maxIter: MaxIter, rng: RNG, 
      fitness: Evaluate[MyEnv,ArrayFormDelta[MyEnv,Int],Int])

    type MyArrayFormDelta = ArrayFormDelta[MyEnv,Int] 
      
    def fitnessImpl(target: ArrayForm)(x: ArrayForm): Int = {
      require( target.size == x.size )
      (0 until target.size ).count { i => target.get(i) != x.get(i) } 
    }

    case class HammingSwapDelta(target: ArrayForm, index1: Int, index2: Int) extends Delta[ArrayForm,Int] {
      override def apply(base: ArrayForm): (ArrayForm,Int) = {

        val index1Old = if( base.get(index1) == target.get(index1) ) 0 else 1
        val index2Old = if( base.get(index2) == target.get(index2) ) 0 else 1          
        val index1New = if( base.get(index2) == target.get(index1) ) 0 else 1
        val index2New = if( base.get(index1) == target.get(index2) ) 0 else 1
        
        val deltaChanges = ( index1New + index2New ) - ( index1Old + index2Old ) 
        val result = base.clone()
        result.transpose(index1, index2)
        (result,deltaChanges)
      }
    }

  /////////////////////////////////
    
    case class HammingEval[Env](target: ArrayForm) extends Evaluate[Env,ArrayForm,Int] {
      override def apply(x: ArrayForm): State[Env,Int] = State[Env,Int] { env =>
        (env,fitnessImpl(target)(x))
      }
    }

    case class HammingDeltaEval[Env](target: ArrayForm) extends EvaluateDelta[Env,ArrayForm,Int] {
      override def apply(x: ArrayFormDelta[Env,Int]): State[Env,Int] = State[Env,Int] { env =>
        val fitnessByDeltaEval = x.updatedValue
//        if( fitnessByDeltaEval == fitnessImpl(target)( x.updatedSol ) )
//          Diag.println("delta eval OK " + fitnessByDeltaEval )
          
//        if( fitnessByDeltaEval != fitnessImpl(target)( x.updatedSol ) ) {
//          Diag.println( x )
//          Diag.println( fitnessByDeltaEval )
//          Diag.println( fitnessImpl(target)( x.updatedSol ) )
//        }
        assert( fitnessByDeltaEval == fitnessImpl(target)( x.updatedSol ) )        
        (env,fitnessByDeltaEval)
      }
    }
    
  	val iterLens: Lens[MyEnv, Iter] = monocle.macros.GenLens[MyEnv] { _.iter }
	  val maxIterLens: Lens[MyEnv, MaxIter] = monocle.macros.GenLens[MyEnv] { _.maxIter }
	  val rngLens: Lens[MyEnv, RNG] = monocle.macros.GenLens[MyEnv] { _.rng }	  
	  val fitnessLens: Lens[MyEnv, Evaluate[MyEnv,MyArrayFormDelta,Int]] = monocle.macros.GenLens[MyEnv] { _.fitness }
    
    val perturb: Perturb[MyEnv,MyArrayFormDelta] = RandomSwap(rngLens, 
      (index1,index2) => HammingSwapDelta(target,index1,index2) )
    
    val accept: Accept[MyEnv,MyArrayFormDelta] = AcceptImprovingOrEqual(isMinimizing=true, scala.math.Ordering.Int, fitnessLens)
    val isFinished: Condition[MyEnv,MyArrayFormDelta] = IsFinished.IterGreaterThanMaxIter(iterLens,maxIterLens)
		
	  val search = IteratedPerturbReturnLast(iterLens,perturb,accept,isFinished)
	  val fitness = HammingEval[MyEnv](target)
	  val fitnessDelta: EvaluateDelta[MyEnv,ArrayForm,Int] = HammingDeltaEval[MyEnv](target)	  
	  
	  val initialS = ArrayFormDelta[MyEnv,Int](source,fitness)
	  val initialEnv = MyEnv(Iter(0),MaxIter(numIter),KnuthLCG64(seed), fitnessDelta )
	  
	  val searchS = for {
	    initial <- initialS
	    result <- search( initial )
	  } yield { result }
	  
	  val (finalEnv,solution) = searchS.run( initialEnv ).value
	  (solution.updatedSol,fitnessImpl(target)(solution.updatedSol))
  }
  
  /////////////////////////////////  
  
  def main(args: Array[String]): Unit = {

    val InstanceSize = 3
    val targetImpl = (0 until InstanceSize).toArray
	  val target = new ArrayForm( targetImpl:_* )
    val source = new ArrayForm( targetImpl.reverse:_* )
    Diag.println( search(source,target,numIter = 100, seed = 0xDEADBEEF) )
    
// Diag.println( s"solution: ${solution.updated}" )
// Diag.println( fitnessImpl(target)( solution.updated ) )
    
  }

///////////////////////////////////
  
} // object PermutationMimicry {

// End ///////////////////////////////////////////////////////////////

