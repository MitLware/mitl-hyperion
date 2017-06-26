package org.mitlware.hyperion3.mutable

import scala.util.Random

import org.mitlware._
import org.mitlware.mutable._

package object accept {

  def MetropolisHastings[S,V<:Comparable[V]]( 
    iterCount: ReadOnly[Long], 
    random: ReadWrite[Random],
    temperature : ReadOnly[Double],
    eval: Evaluate.Directional[S,V],
    schedule: ( Double, Long ) => Double )( implicit ev: V => Double ) : Accept[S] = {
    (incumbent: S, incoming: S) => {
      
      val incumbentValue = eval(incumbent)      
      val incomingValue = eval(incoming)
      val acceptIncoming = metaxa.temperature.MetropolisHastings.apply( 
        incomingValue, incumbentValue, 
        schedule.apply( temperature.get, iterCount.get ),
        eval.isMinimizing, random.get.self )
    
      if( acceptIncoming ) 
        incoming 
      else 
        incumbent
    }
  }
}

// End ///////////////////////////////////////////////////////////////
