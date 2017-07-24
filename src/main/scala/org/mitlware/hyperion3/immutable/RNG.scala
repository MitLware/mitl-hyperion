package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

///////////////////////////////////

trait RNG {
  def asInt:  Int  
  def asLong: Long
  def advance: RNG
}

///////////////////////////////////

case class KnuthLCG64(override val asLong: Long) extends RNG {
  
  override val asInt: Int = asLong.toInt
  
  override def advance: RNG =
    copy(asLong = asLong * 6364136223846793005L + 1442695040888963407L)
}

///////////////////////////////////

object RNG {

  def nextLong[Env](rngLens: Lens[Env,RNG]): State[Env,Long] = State[Env,Long] { env =>
    val newRNG = rngLens.get(env).advance
    (rngLens.set(newRNG)(env),newRNG.asLong)
  }

  def nextInt[Env](rngLens: Lens[Env,RNG]): State[Env,Int] = State[Env,Int] { env =>
    val newRNG = rngLens.get(env).advance
    (rngLens.set(newRNG)(env),newRNG.asInt)
  }
  
  def nextInt[Env](rngLens: Lens[Env,RNG], ub: Int): State[Env, Int] = {
    require( ub > 0 )
    nextInt[Env](rngLens).map { r => ( ( r.toInt % ub ) + ub ) % ub }
  }
  
  def nextInt[Env](rngLens: Lens[Env,RNG], lb: Int, ub: Int): State[Env, Int] = {
    require( lb >= 0 && lb < ub )
    nextInt[Env](rngLens, (ub - lb ) + 1).map { _ + lb } 
  }
  
  def nextBoolean[Env](rngLens: Lens[Env,RNG]): State[Env, Boolean] = 
    nextLong[Env](rngLens).map { _ > 0 }
  
  def nextDouble[Env](rngLens: Lens[Env,RNG]): State[Env, Double] = 
    nextLong[Env](rngLens).map { java.lang.Double.longBitsToDouble(_) }
}

// End ///////////////////////////////////////////////////////////////
