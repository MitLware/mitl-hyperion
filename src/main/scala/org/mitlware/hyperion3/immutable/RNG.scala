package org.mitlware.hyperion3.immutable

import cats._, cats.data._, cats.implicits._
import monocle.Lens
// import scalaz.State
// import scalaz.Lens

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

/**
 * Simple generator that passes the Die-hard statistical test suite
 */
/*****
case class XORShift(a: Int, b: Int, c: Int, d: Int, e: Int) extends RNG {
  override def nextInt : (XORShift, Int) = {
    val ep = a ^ (a << 11)
    val n = d ^ (d >> 19) ^ (ep ^ (ep >> 8))
    val nextGen = XORShift( b, c, d, n, ep )
    (nextGen,n)
  }
}

object XORShift {
  def withSeed( seed : Int ) = XORShift( 123456789, 362436069, 521288629, 88675123, seed )
}
*****/

///////////////////////////////////

object RNG {

//  def nextInt(rng: RNG): State[RNG,Int] = State[RNG,Int] { env =>
//    val newRNG = rng.advance
//    (newRNG,newRNG.asInt)
//  }

//  def nextInt(rng: RNG, ub: Int): State[RNG, Int] = {
//    require( ub > 0 )
//    nextInt(rng).map { r => ( ( r.toInt % ub ) + ub ) % ub }
//  }
  
// Need implicit Traverse[List] 

//    import scalaz.std.list.listInstance 
//
//    val nextInt = State[RNG, Int]  { (rng:RNG) => 
//      val newRNG = rng.advance
//      (newRNG,newRNG.asInt)
//    } 
//
//    def nextInt(ub: Int): State[RNG, Int] =   
//      nextInt.map { r => ( ( r.toInt % ub ) + ub ) % ub }
//    
//    val rng42 = KnuthLCG64(42) 
//
//    val applicative = scalaz.Applicative[({type l[Int] = State[RNG,Int]})#l] 
//
//    // To generate the first 5 Random integers 
//    val chain: State[RNG, List[Int]] = applicative.sequence(List.fill(5)(int)) 
//    val chainResult: (RNG, List[Int]) = chain.run(rng42) 
//    chainResult._2.foreach(println) 
//    
////  def nextInts(rng: RNG, ub: Int, num: Int): State[RNG, Int] = {
////    require( ub > 0 )
////    val xx = (0 until num ).map { _ => 
////      nextInt(rng).map { r => ( ( r.toInt % ub ) + ub ) % ub } }
////  }
  
  /////////////////////////////////
  
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
