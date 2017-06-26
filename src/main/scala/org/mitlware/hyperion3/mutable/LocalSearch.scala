package org.mitlware.hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._

import scala.util.Random

//////////////////////////////////////////////////////////////////////

object LocalSearch {

  def returnLast[S]( perturb: Perturb[S], 
    accept: Accept[S],
    isFinished: IsFinished[S], 
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long]) : IteratedPerturb[S] =     
      apply(perturb, accept, isFinished, Option.empty, iterCount, startTime)

  def returnBest[S]( perturb: Perturb[S], 
    accept: Accept[S],
    isFinished: IsFinished[S], prefer: Prefer[S],
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long]) : IteratedPerturb[S] =     
      apply(perturb, accept, isFinished, Option.apply(prefer), iterCount,startTime)

  def randomSearchReturnLast[S](create: Create[S], 
    isFinished: IsFinished[S], 
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long],
    random: ReadWrite[Random]) : IteratedPerturb[S] =     
      apply(Perturb.from(create), Accept.always[S], isFinished, Option.empty, iterCount,startTime)
  
  /** @see "Essentials of Metaheuristics" 2nd Edition, Algorithm 9 */

  // TODO: randomSearchReturnBest

  ///////////////////////////////

  def simulatedAnnealing[S,V<:Comparable[V]]( perturb: Perturb[S], 
    isFinished: IsFinished[S],
    prefer: Prefer[S],
    eval: Evaluate.Directional[S,V],
    schedule: ( Double, Long ) => Double,
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long],    
    temperature: ReadWrite[Double], 
    random: ReadWrite[Random] )( implicit ev: V => Double ) : IteratedPerturb[S] = {
    
    val accept = org.mitlware.hyperion3.mutable.accept.MetropolisHastings( iterCount,
      random, temperature, eval, schedule )( ev ) 
    returnBest(perturb, accept, isFinished, prefer, iterCount,startTime)
  }

  /////////////////////////////////
  
  import org.apache.commons.collections15.buffer.CircularFifoBuffer
  
  def strictTabu[S,Feature]( perturb: Perturb[S], 
    isFinished: IsFinished[S],
    prefer: Prefer[S],
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long], 
    featureFn: ( S, S ) => Set[Feature],
    tabuList: ReadWrite[CircularFifoBuffer[Set[Feature]]]
  ) : IteratedPerturb[S] = {
    
    val accept : Accept[S] = org.mitlware.hyperion3.mutable.accept.Tabu.strict(featureFn, tabuList) _
    returnBest(perturb, accept, isFinished, prefer, iterCount,startTime)
  }
  
  /////////////////////////////////
  
  def randomRestartLS[S]( perturb: Perturb[S], 
    accept: Accept[S],
    isFinished: IsFinished[S], prefer: Prefer[S],
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long]) : IteratedPerturb[S] =     
      apply(perturb, accept, isFinished, Option.apply(prefer), iterCount,startTime)
  
  /////////////////////////////////

  def apply[S]( perturb: Perturb[S], 
    accept: Accept[S], 
    isFinished: IsFinished[S], 
    prefer: Option[Prefer[S]], 
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long] ) : IteratedPerturb[S] =
      new IteratedPerturb[S] {
        override def apply( s : S ) : S = run( s, 
          perturb, accept, isFinished, prefer, iterCount, startTime )
      }

  /////////////////////////////////
  
  private def run[S]( initial: S,
    perturb: Perturb[S],
    accept: Accept[S], 
    isFinished: IsFinished[S],
    prefer: Option[Prefer[S]], 
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long] ) : S = {

    var iter = 0L
    iterCount.set( iter )
    startTime.set( System.currentTimeMillis() )
    
    ///////////////////////////////
    
    var incumbent = initial    
    var best = prefer.map( ignore => incumbent )

    var finished = isFinished( incumbent )   
    while( !finished ) {
      
      var incoming = perturb( incumbent )
//jeep.lang.Diag.println( "incumbent: " + incumbent )
//jeep.lang.Diag.println( "incoming: " + incoming )
      incumbent = accept( incumbent, incoming )
//jeep.lang.Diag.println( "accepted: " + incumbent )

    best = for( p <- prefer; bst <- best ) yield p.prefer( bst, incumbent )

  //      jeep.lang.Diag.println( "pref: " + prefer.get.prefer( best.get, incumbent ) )

//jeep.lang.Diag.println( "best: " + best )

      finished = isFinished( incumbent )
      
      iter = iter + 1
      iterCount.set( iter )
    }
    
    best.getOrElse( incumbent )
  }
}

// End ///////////////////////////////////////////////////////////////