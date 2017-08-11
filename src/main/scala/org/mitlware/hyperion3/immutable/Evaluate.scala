package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////

trait Evaluate[Env,Sol,Value] {
  def apply(incumbent: Sol): State[Env,Value]
}

object Evaluate {

  abstract class Directional[Env,Sol,Value](
    val direction: org.mitlware.SearchDirection)(
      implicit val ordering: Ordering[Value]) extends Evaluate[Env,Sol,Value] {
  
    val isMinimizing: Boolean = direction == org.mitlware.SearchDirection.MINIMIZING
  
    override def apply(incumbent: Sol): State[Env,Value]
  }
  
  object Directional {
		 
    def minimizing[E,S,V](eval: Evaluate[E,S,V])(implicit ordering: Ordering[V]): Directional[E,S,V] = 
      new Directional[E,S,V]( org.mitlware.SearchDirection.MINIMIZING ) {
        override def apply(incumbent: S): State[E,V] = eval(incumbent)
      }

    def maximizing[E,S,V](eval: Evaluate[E,S,V])(implicit ordering: Ordering[V]): Directional[E,S,V] = 
      new Directional[E,S,V]( org.mitlware.SearchDirection.MAXIMIZING ) {
        override def apply(incumbent: S): State[E,V] = eval(incumbent)
    }
  }
  
  /////////////////////////////////
}

///////////////////////////////////

//trait Delta[T] {
//  def apply(incumbent: T): T  
//}
//
//case class NoDelta[T](incoming: T) extends Delta[T] {
//  def apply(incumbent: T): T = incoming  
//}

trait Delta[Sol,Value] {
  def apply(base: Sol): (Sol,Value)    
}

///////////////////////////////////

case class DeltaEvaluated[Env,Sol,Value](
  baseSol: Sol,
  baseValue: Value,
  deltas: List[Delta[Sol,Value]])(implicit ev: Semigroup[Value]) {
  
  // type DeltaType = Delta
  
  lazy val (updatedSol,updatedValue) = DeltaEvaluated.applyDeltas(baseSol,baseValue,deltas)
  
  def update(newDeltas: List[Delta[Sol,Value]]): DeltaEvaluated[Env,Sol,Value] = 
    DeltaEvaluated(updatedSol,updatedValue,newDeltas)      
}

///////////////////////////////////

object DeltaEvaluated {

  def applyDeltas[Sol,Value](base: Sol, baseValue: Value, deltas: List[Delta[Sol,Value]])(
    implicit ev: Semigroup[Value]): (Sol,Value) = 
      deltas.foldLeft( (base,baseValue) ) { case ((accSol,accValue),delta) => 
        val (newSol,deltaValue) = delta.apply( accSol )
        (newSol,ev.combine(accValue,deltaValue))
      }
}

///////////////////////////////////

trait EvaluateDelta[Env,Sol,Value] extends Evaluate[Env,DeltaEvaluated[Env,Sol,Value],Value] {
  def apply(incumbent: DeltaEvaluated[Env,Sol,Value]): State[Env,Value]
}

// End ///////////////////////////////////////////////////////////////
