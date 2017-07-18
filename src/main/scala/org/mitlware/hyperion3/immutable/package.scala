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
  def apply(incumbent: Sol): State[Env,Sol]
}

///////////////////////////////////

trait Evaluate[Env,Sol,Value] {
  def apply(incumbent: Sol): State[Env,Value]
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

} // package object immutable {

// End ///////////////////////////////////////////////////////////////
