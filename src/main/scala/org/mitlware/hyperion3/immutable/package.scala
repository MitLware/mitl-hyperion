package org.mitlware.hyperion3

import cats.data.State
import monocle._

///////////////////////////////////

package object immutable {
  
///////////////////////////////////
  
case class Iter(asLong: Long) 
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

trait Perturb[Env,Sol] {
  // def apply(incumbent: Sol): State[Env,Delta[Sol]]
  def apply(incumbent: Sol): State[Env,Sol]
}

///////////////////////////////////

trait Accept[Env,Sol]  {
  def apply(incumbent: Sol,incoming: Sol): State[Env,Sol]
}

///////////////////////////////////

trait Condition[Env,Arg] {
  def apply(arg: Arg): State[Env,Boolean]
}

///////////////////////////////////

trait Evaluate[Env,Sol,Value] {
  def apply(incumbent: Sol): State[Env,Value]
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
  deltas: List[Delta[Sol,Value]])(implicit ev: cats.Semigroup[Value]) {
  
  // type DeltaType = Delta
  
  lazy val (updatedSol,updatedValue) = DeltaEvaluated.applyDeltas(baseSol,baseValue,deltas)
  
  def update(newDeltas: List[Delta[Sol,Value]]): DeltaEvaluated[Env,Sol,Value] = 
    DeltaEvaluated(updatedSol,updatedValue,newDeltas)      
}

///////////////////////////////////

object DeltaEvaluated {

  def applyDeltas[Sol,Value](base: Sol, baseValue: Value, deltas: List[Delta[Sol,Value]])(
    implicit ev: cats.Semigroup[Value]): (Sol,Value) = 
      deltas.foldLeft( (base,baseValue) ) { case ((accSol,accValue),delta) => 
        val (newSol,deltaValue) = delta.apply( accSol )
        (newSol,ev.combine(accValue,deltaValue))
      }
}

///////////////////////////////////

trait EvaluateDelta[Env,Sol,Value] extends Evaluate[Env,DeltaEvaluated[Env,Sol,Value],Value] {
  def apply(incumbent: DeltaEvaluated[Env,Sol,Value]): State[Env,Value]
}

///////////////////////////////////

} // package object immutable {

// End ///////////////////////////////////////////////////////////////
