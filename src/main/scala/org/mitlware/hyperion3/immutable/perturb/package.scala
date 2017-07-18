package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

import org.mitlware.hyperion3.immutable._

///////////////////////////////////

package object perturb {

///////////////////////////////////

case class IteratedPerturbation[Env,Sol](
  iter: Lens[Env,Iter], 
  perturb: Perturb[Env,Sol], 
  accept: Accept[Env,Sol], 
  isFinished: Condition[Env,Sol]) extends Perturb[Env,Sol] {
  
  def loop(incumbent: Sol): State[Env, Sol] = for {
    _ <- State.modify[Env]( env => iter.modify(x => Iter(x.asLong + 1))(env) ); 
    incoming <- perturb(incumbent);
    accepted <- accept(incumbent,incoming);
    finished <- isFinished(accepted);
    result <- if(finished) State.pure[Env,Sol](accepted) else loop(accepted)
  } yield { result }

  def apply(incumbent: Sol): State[Env,Sol] = loop(incumbent)
}
  
///////////////////////////////////  
  
} // package object perturb {

// End ///////////////////////////////////////////////////////////////
