package org.mitlware.hyperion3.immutable.perturb.permutation

import cats.data.State
import cats.Semigroup
import cats.implicits._

import monocle.Lens

import org.mitlware.hyperion3.immutable._
import org.mitlware.hyperion3.immutable.perturb._
import org.mitlware.hyperion3.immutable.accept._
import org.mitlware.hyperion3.immutable.isfinished._		

import org.mitlware.solution.permutation.ArrayForm

import org.mitlware.Diag

///////////////////////////////////

object EvaluateDelta {
  
///////////////////////////////////  
  
type ArrayFormDelta[Env,Value] = DeltaEvaluated[Env,ArrayForm,Value]

object ArrayFormDelta {
  
  def apply[Env,Value](
    base: ArrayForm,
    eval: Evaluate[Env,ArrayForm,Value])(
    implicit ev: Semigroup[Value]): State[Env,DeltaEvaluated[Env,ArrayForm,Value]] = 
      for { baseValue <- eval(base) } yield 
      DeltaEvaluated( base, baseValue, List.empty[Delta[ArrayForm,Value]])
}

///////////////////////////////////

case class RandomSwap[Env,Value](rngLens: Lens[Env,RNG], 
  mkDelta: (Int,Int) => Delta[ArrayForm,Value] ) extends Perturb[Env,ArrayFormDelta[Env,Value]] {

  override def apply(x: ArrayFormDelta[Env,Value]): State[Env,ArrayFormDelta[Env,Value]] = for {
    index1 <- RNG.nextInt(rngLens, x.baseSol.size );
    index2 <- RNG.nextInt(rngLens, x.baseSol.size )
  } yield x.update( List( mkDelta(index1,index2) ) )
}
  
///////////////////////////////////

case class RandomReinsert[Env,Value](rngLens: Lens[Env,RNG], 
  mkDelta: (Int,Int) => Delta[ArrayForm,Value] ) extends Perturb[Env,ArrayFormDelta[Env,Value]] {

  override def apply(x: ArrayFormDelta[Env,Value]): State[Env,ArrayFormDelta[Env,Value]] = for {
    index1 <- RNG.nextInt(rngLens, x.baseSol.size );
    index2 <- RNG.nextInt(rngLens, x.baseSol.size )
  } yield x.update( List( mkDelta(index1,index2) ) )
}

///////////////////////////////////

case class RandomFlip[Env,Value](rngLens: Lens[Env,RNG], 
  mkDelta: (Int,Int) => Delta[ArrayForm,Value] ) extends Perturb[Env,ArrayFormDelta[Env,Value]] {

  override def apply(x: ArrayFormDelta[Env,Value]): State[Env,ArrayFormDelta[Env,Value]] = for {
    index1 <- RNG.nextInt(rngLens, x.baseSol.size );
    index2 <- RNG.nextInt(rngLens, x.baseSol.size )
  } yield x.update( List( mkDelta(index1,index2) ) )
}

///////////////////////////////////
  
} // object EvaluateDelta {

// End ///////////////////////////////////////////////////////////////
