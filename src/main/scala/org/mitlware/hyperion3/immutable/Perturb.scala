package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////

trait Perturb[Env,Sol] {
  def apply(incumbent: Sol): State[Env,Sol]
}

///////////////////////////////////

case class BestImproving[Env,Sol,Value](
  l: Locality[Env,Sol],
  eval: Lens[Env, Evaluate.Directional[Env,Sol,Value]]) extends Perturb[Env,Sol] {
  
  override def apply(incumbent: Sol): State[Env,Sol] = ???  
  /****
  override def apply(incumbent: Sol): State[Env,Sol] = for {
    incumbentValue <- eval(incumbent);
    neighbours <- l(incumbent);
    values <- neighbours.traverse { s => eval(s) }
  } yield { 
    implicit val ordering = eval.ordering
    val bestFrom = ( incumbent, incumbentValue ) :: neighbours.zip( values )
    val (bestSol,bestValue) = if( eval.isMinimizing ) 
      bestFrom.minBy { _._2 } 
    else 
      bestFrom.maxBy { _._2 } 
    bestSol
  }
*****/
}

///////////////////////////////////

object Perturb {
  
  def fromCreate[Env,Sol](create: Create[Env,Sol]): Perturb[Env,Sol] = 
    new Perturb[Env,Sol] {
      override def apply(ignore: Sol): State[Env,Sol] = 
        for { result <- create.apply } yield { result } 
    }
}

// End ///////////////////////////////////////////////////////////////
