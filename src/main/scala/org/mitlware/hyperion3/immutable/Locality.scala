package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////

trait Locality[Env,Sol] {
  def apply(incumbent: Sol): State[Env,List[Sol]]
}

///////////////////////////////////

case class SamplingLocality[Env,Sol](base: Locality[Env,Sol], numSamples: Int, rngLens: Lens[Env,RNG]) 
extends Locality[Env,Sol] {
  require( numSamples > 0 )

  override def apply(incumbent: Sol): State[Env,List[Sol]] = 
    for {
      neighbours <- base(incumbent);  
      indices <- List.fill( numSamples ) { 
        RNG.nextInt(rngLens, neighbours.length) }.sequenceU
    } yield { 
     indices.map { i => neighbours(i) } 
    }
}

// End ///////////////////////////////////////////////////////////////
