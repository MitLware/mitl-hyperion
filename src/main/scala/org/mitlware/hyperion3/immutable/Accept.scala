package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////

trait Accept[Env,Sol]  {
  def apply(incumbent: Sol,incoming: Sol): State[Env,Sol]
}

object Accept {

  def always[Env,Sol] = new Accept[Env,Sol] {
    override def apply(incumbent: Sol,incoming: Sol) = 
      State[Env,Sol] { env =>	(env,incoming) }
  }
}

///////////////////////////////////

case class AcceptImproving[Env,Sol,Value](
  eval: Lens[Env, Evaluate.Directional[Env,Sol,Value]]) extends Accept[Env,Sol] {
  
  override def apply(incumbent: Sol, incoming: Sol): State[Env,Sol] = for {
    env <- State.get[Env];
    ordering = eval.get(env).ordering
    isMinimizing = eval.get(env).isMinimizing    
    incumbentValue <- eval.get(env).apply(incumbent);
    incomingValue <- eval.get(env).apply(incoming) 
  } yield { 
    if( isMinimizing ) { 
      if( ordering.lt( incomingValue, incumbentValue ) ) incoming else incumbent      
    } 
    else { 
      if( ordering.gt( incomingValue, incumbentValue ) ) incoming else incumbent      
    }
  } 
}

///////////////////////////////////

case class AcceptImprovingOrEqual[Env,Sol,Value](
  isMinimizing: Boolean, 
  ordering: Ordering[Value],
  evaluate: monocle.Lens[Env, Evaluate[Env,Sol,Value]]) extends Accept[Env,Sol] {
  
  override def apply(incumbent: Sol, incoming: Sol): State[Env,Sol] = for {
    env <- State.get[Env]
    incumbentValue <- evaluate.get(env).apply(incumbent); // FIXME
    incomingValue <- evaluate.get(env).apply(incoming)  // FIXME
  } yield { 
    if( isMinimizing ) { 
      if( ordering.lteq( incomingValue, incumbentValue ) ) incoming else incumbent 
    } 
    else { 
      if( ordering.gteq( incomingValue, incumbentValue ) ) incoming else incumbent 
    }
  } 
}

///////////////////////////////////

trait CoolingSchedule[Env] {
  def apply: State[Env,Temperature]
}

case class GeometricCoolingSchedule[Env](ratio: CoolingRatio, temperature: monocle.Lens[Env,Temperature]) extends CoolingSchedule[Env] {
  override def apply: State[Env,Temperature] = State[Env,Temperature] { env =>
    val current = temperature.get(env)
    val newEnv = temperature.set(Temperature( current.asDouble * ratio.asDouble ))(env)
    (newEnv, current)
  }
}

case class LinearCoolingSchedule[Env](initialTemperature: Temperature,
    temperature: monocle.Lens[Env,Temperature],iter: Lens[Env,Iter],maxIter: Lens[Env,MaxIter]) extends CoolingSchedule[Env] {
  override def apply: State[Env,Temperature] = State[Env,Temperature] { env =>
    // val current = temperature.get(env)
    val newTemperature = Temperature( org.mitlware.support.math.LinearInterpolation.apply(iter.get(env).asLong, 
      0, 1 + maxIter.get(env).asLong, 
      initialTemperature.asDouble, 0.0 ) )
    val newEnv = temperature.set(newTemperature)(env)
    (newEnv, newTemperature)
  }
}

///////////////////////////////////

case class AcceptMetropolisHastings[Env,Sol](isMinimizing: Boolean, rngLens: Lens[Env, RNG], evaluate: Lens[Env, Evaluate[Env,Sol,Double]], schedule: CoolingSchedule[Env]) extends Accept[Env,Sol] {
  override def apply(incumbent: Sol, incoming: Sol): State[Env,Sol] = for {
    env <- State.get[Env]
    incumbentValue <- evaluate.get(env).apply(incumbent);
    incomingValue <- evaluate.get(env).apply(incoming) 
    temperature <- schedule.apply;
    val acceptProb: Double = 1.0 / ( 1.0 + Math.exp(( if (isMinimizing) incomingValue - incumbentValue else incumbentValue - incomingValue) ) / temperature.asDouble )
  } yield ??? // if( RNG.nextDouble < acceptProb ) incoming else incumbent // FIXME
}

// End ///////////////////////////////////////////////////////////////
