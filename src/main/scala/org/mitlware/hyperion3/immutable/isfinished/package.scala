package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

///////////////////////////////////

package object isfinished {
  
///////////////////////////////////

case class IterGreaterThanMaxIter[Env,Sol](
  iter: Lens[Env,Iter],
  maxIter: Lens[Env,MaxIter]) extends Condition[Env,Sol] {
  
  def apply(incumbent: Sol): State[Env,Boolean] = State[Env,Boolean] { env =>
    (env, iter.get(env).asLong > maxIter.get(env).asLong )
  }
}

///////////////////////////////////

} // package object isfinished {

// End ///////////////////////////////////////////////////////////////
