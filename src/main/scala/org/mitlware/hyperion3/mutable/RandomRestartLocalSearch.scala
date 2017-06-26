package org.mitlware.hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._

import scala.util.Random

//////////////////////////////////////////////////////////////////////

object RandomRestartLocalSearch {

  import scala.collection.immutable.NumericRange
  
  private def randomTimeoutIsFinished[S]( startTimeInner: ReadWrite[Long], 
    timeoutRangeMillis: NumericRange[Long], 
    random: ReadWrite[Random] ) : () => IsFinished[S] = {
  
    val MaxTimeInMilliseconds = ( random.get.nextLong() % timeoutRangeMillis.start ) + timeoutRangeMillis.end    
    () => (s: S) => System.currentTimeMillis() - startTimeInner.get>= MaxTimeInMilliseconds
  }

  /////////////////////////////////
    
  def essentials2EdAlg10[S]( perturb: Perturb[S],
    accept: Accept[S], 
    isFinishedOuter: IsFinished[S],
    prefer: Prefer[S], 
    iterCountOuter: ReadWrite[Long],
    startTimeOuter: ReadWrite[Long],
    createState : () => S,
    timeoutRangeMillis: NumericRange[Long],
    iterCountInner: ReadWrite[Long],
    startTimeInner: ReadWrite[Long],
    random: ReadWrite[Random]) : IteratedPerturb[S] = {
 
    apply( perturb, accept, isFinishedOuter, prefer, 
      iterCountOuter, startTimeOuter, 
      randomTimeoutIsFinished[S]( startTimeInner,
          timeoutRangeMillis, random ),
      createState, iterCountInner, startTimeInner )    
  }
  
  /////////////////////////////////

  def apply[S]( perturb: Perturb[S],
    accept: Accept[S], 
    isFinishedOuter: IsFinished[S],
    prefer: Prefer[S], 
    iterCountOuter: ReadWrite[Long],
    startTimeOuter: ReadWrite[Long],
    createIsFinished : () => IsFinished[S],
    createState : () => S,    
    iterCountInner: ReadWrite[Long],
    startTimeInner: ReadWrite[Long] ) : IteratedPerturb[S] =
      new IteratedPerturb[S] {
        override def apply( s : S ) : S = run( s, 
            perturb, accept, isFinishedOuter, prefer, 
            iterCountOuter, startTimeOuter, createIsFinished,
            createState, iterCountInner, startTimeInner )
      }
  
  /////////////////////////////////
  
  private def run[S]( initial: S,
    perturb: Perturb[S],
    accept: Accept[S], 
    isFinishedOuter: IsFinished[S],
    prefer: Prefer[S], 
    iterCountOuter: ReadWrite[Long],
    startTimeOuter: ReadWrite[Long],
    createIsFinished : () => IsFinished[S],
    createState : () => S,    
    iterCountInner: ReadWrite[Long],
    startTimeInner: ReadWrite[Long] ) : S = {

    var iter = 0L
    iterCountOuter.set( iter )
    startTimeOuter.set( System.currentTimeMillis() )
    
    ///////////////////////////////
    
    var incumbent = initial    
    var best = incumbent

    var finished = isFinishedOuter( incumbent )   
    while( !finished ) {

      val isFinishedInner = createIsFinished()
      val innerSearch = LocalSearch.returnBest( 
        perturb, accept, isFinishedInner, prefer, iterCountInner, startTimeInner )
        
      var incoming = innerSearch( createState() )
      
      incumbent = accept( incumbent, incoming )
      best = prefer.prefer( best, incumbent )
        
      finished = isFinishedOuter( incumbent )
      
      iter = iter + 1
      iterCountOuter.set( iter )
    }
    
    best
  }
}

// End ///////////////////////////////////////////////////////////////