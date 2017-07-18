package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

///////////////////////////////////

// FIXME: replace with MersenneTwister

case class RNG(asLong: Long) {
  // for brevity, just use Knuth's 64-bit LCG:      
  def advance: RNG =
    copy(asLong = asLong * 6364136223846793005L + 1442695040888963407L)
}

///////////////////////////////////

object RNG {
  
  def nextLong[Env](rngLens: Lens[Env,RNG]): State[Env,Long] = State[Env,Long] { env =>
    val newRNG = rngLens.get(env).advance
    (rngLens.set(newRNG)(env),newRNG.asLong)
  }
  
  def nextInt[Env](rngLens: Lens[Env,RNG]): State[Env, Int] = 
    nextLong[Env](rngLens).map { _.toInt }
  
  def nextInt[Env](rngLens: Lens[Env,RNG], ub: Int): State[Env, Int] = {
    require( ub > 0 )
    nextLong[Env](rngLens).map { r => ( ( r.toInt % ub ) + ub ) % ub }
  }
  
  def nextBoolean[Env](rngLens: Lens[Env,RNG]): State[Env, Boolean] = 
    nextLong[Env](rngLens).map { _ > 0 }
  def nextDouble[Env](rngLens: Lens[Env,RNG]): State[Env, Double] = 
    nextLong[Env](rngLens).map { java.lang.Double.longBitsToDouble(_) }
}

// End ///////////////////////////////////////////////////////////////
