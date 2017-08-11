package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////  
  
case class IteratedPerturbReturnLast[Env,Sol](
  iter: Lens[Env,Iter], 
  perturb: Perturb[Env,Sol],
  accept: Accept[Env,Sol], 
  isFinished: Condition[Env,Sol]) extends Perturb[Env,Sol] {

  override def apply(incumbent: Sol): State[Env,Sol] = {
    def loop(s: Sol): State[Env, Sol] = for {
      _ <- State.modify[Env]( env => iter.modify ( _.inc )( env ) );
      incoming <- perturb(s);
      accepted <- accept(s,incoming);
      finished <- isFinished(accepted);
      result <- if(finished) State.pure[Env,Sol](accepted) else loop(accepted)
    } yield { result }
    
    loop(incumbent)
  }
}

///////////////////////////////////

case class IteratedPerturbReturnBest[Env,Sol](
  iter: Lens[Env,Iter], 
  order: Lens[Env, Order[Env,Sol]],  
  perturb: Perturb[Env,Sol],
  accept: Accept[Env,Sol], 
  isFinished: Condition[Env,Sol]) extends Perturb[Env,Sol] {

  override def apply(incumbent: Sol): State[Env,Sol] = {
    def loop(s: Sol, bestSoFar: Sol): State[Env, Sol] = for {
      _ <- State.modify[Env]( env => iter.modify ( _.inc )( env ) );
      incoming <- perturb(s);
      env <- State.get[Env];      
      best <- order.get(env).apply(bestSoFar,incoming);
      accepted <- accept(s,incoming);
      finished <- isFinished(accepted);
      result <- if(finished) State.pure[Env,Sol](best) else loop(accepted,best)
    } yield { result }
    
    loop(incumbent,incumbent)
  }
}

///////////////////////////////////

object IteratedPerturb {

  // Hillclimbing
  def essentialsAlgorithm4[Env,Sol,Value](
    iter: Lens[Env,Iter], 
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Value]],    
    perturb: Perturb[Env,Sol],
    isFinished: Condition[Env,Sol]) = 
      IteratedPerturbReturnLast[Env,Sol](
        iter, 
        perturb, 
        AcceptImproving(eval), 
        isFinished )

  /////////////////////////////////
        
  // Steepest Ascent Hill-Climbing        
  def essentialsAlgorithm5[Env,Sol,Value](
    iter: Lens[Env,Iter], 
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Value]],    
    locality: Locality[Env,Sol],
    numSamples: Int,
    rngLens: Lens[Env,RNG],    
    isFinished: Condition[Env,Sol]) = 
      IteratedPerturbReturnLast[Env,Sol](
        iter, 
        BestImproving( SamplingLocality(locality, numSamples, rngLens), eval ), 
        AcceptImproving(eval), 
        isFinished )

  /////////////////////////////////

  // Random Search        
  def essentialsAlgorithm9[Env,Sol,Value](
    iter: Lens[Env,Iter], 
    order: Lens[Env, Order[Env,Sol]],    
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Value]],    
    create: Create[Env,Sol],
    isFinished: Condition[Env,Sol]) = 
      IteratedPerturbReturnBest[Env,Sol](
        iter, 
        order,
        Perturb.fromCreate(create), 
        Accept.always, 
        isFinished )
        
  /////////////////////////////////        
  
}

// End ///////////////////////////////////////////////////////////////
