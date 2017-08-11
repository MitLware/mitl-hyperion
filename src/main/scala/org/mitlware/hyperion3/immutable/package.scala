package org.mitlware.hyperion3

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens
//import scalaz.State
//import scalaz.Lens
//import scalaz.Semigroup

///////////////////////////////////

package object immutable {
  
///////////////////////////////////
  
case class Iter(asLong: Long) {
  def inc = Iter(asLong + 1)  
}

case class MaxIter(asLong: Long)

case class Temperature(asDouble: Double) {
  require(asDouble >= 0)  
}

case class CoolingRatio(asDouble: Double) {
  require(asDouble >= 0)  
}

case class MutationStrength(asDouble: Double) {
  require(asDouble >= 0 && asDouble <= 1.0)  
}

///////////////////////////////////

trait Condition[Env,Arg] {
  def apply(arg: Arg): State[Env,Boolean]
}

///////////////////////////////////

object IsFinished {

  case class IterGreaterThanMaxIter[Env,Sol](
    iter: Lens[Env,Iter],
    maxIter: Lens[Env,MaxIter]) extends Condition[Env,Sol] {
  
    def apply(incumbent: Sol): State[Env,Boolean] = State[Env,Boolean] { env =>
      (env, iter.get(env).asLong > maxIter.get(env).asLong )
    }
  }
  
}

///////////////////////////////////

trait Order[Env,Entity] {
  def apply(a: Entity,b: Entity): State[Env,Entity]
  
 def reduce[A, B, F[_]](fa: F[A])(f: A => B)(
    implicit FF: Traverse[F], BB: Monoid[B]): B = {
      val x = fa traverseU { (a: A) => Const((f(a))) }
      x.getConst
  }

//  def compare(t: Entity, bestLens: Lens[Env,Entity]) : State[Env,Entity] = State { s =>
//    val b = s.getBestSoFar
//    val ord = s.getOrder
//    val better = ord.better(t, b)
//    (s.setBestSoFar(better),better)
//  }
  
//  def zz: Unit = {
//    val l = List.empty[Entity]
//    implicit val orderMonoid: Monoid[Entity] = ???
//    val xx = reduce( l ) { x => x }
//  }
//
//  def bestSS(l: List[Char]): State[Env,Int] = {
//    implicit val orderMonoid: Monoid[Int] = ???    
//    for {
//      result <- reduce( l ) { c: Char => c.toInt }
//    } yield result      
//  }
  
//  def best(l: List[Entity]): State[Env,Option[Entity]] = {
//    implicit val orderMonoid = ???    
//    for {
//      result <- reduce( List('a', 'b', 'c') ) { c: Char => c.toInt };
//      ???
//    } yield result      
//  }
}

///////////////////////////////////

trait Create[Env,Entity] {
  def apply: State[Env,Entity]
}

///////////////////////////////////

trait Recombine2[Env,Sol] {
  def apply(a: Sol, b: Sol): State[Env,Sol]
}

///////////////////////////////////

} // package object immutable {

// End ///////////////////////////////////////////////////////////////
