
package hyperion3.immutable

/************************

import org.mitlware.immutable._

//////////////////////////////////////////////////////////////////////

object LocalSearch {

  def returnLast[S,E]( perturb: Perturb[S,E], 
    accept: Accept[S,E],
    isFinished: IsFinished[S,E]) : IteratedPerturb[S,E] =     
      new LocalSearch( perturb, accept, isFinished, Option.empty )

  def returnBest[S,E]( perturb: Perturb[S,E], 
    accept: Accept[S,E],
    isFinished: IsFinished[S,E], prefer: Prefer[S,E]) : IteratedPerturb[S,E] =     
      new LocalSearch( perturb, accept, isFinished, Option.apply(prefer) )

  def randomSearchReturnLast[S,E](create: Create[S,E], 
    isFinished: IsFinished[S,E], random: scala.util.Random) : IteratedPerturb[S,E] =     
      new LocalSearch( Perturb.from(create, random), Accept.always, isFinished, Option.empty )
  
  /** @see "Essentials of Metaheuristics" 2nd Edition, Algorithm 9 */

  def randomSearchReturnBest[S,E](create: Create[S,E], 
    isFinished: IsFinished[S,E], random: scala.util.Random, prefer: Prefer[S,E]) : IteratedPerturb[S,E] =     
      new LocalSearch( Perturb.from(create, random), Accept.always, isFinished, Option.apply(prefer) )

  ///////////////////////////////
  
  /** @see "Essentials of Metaheuristics" 2nd Edition, Algorithm 9 */

  /******
  
  public static < S, V extends DirectedValue< Double, V >, E > 
  IteratedPerturb< S, E > 
  SimulatedAnnealing( Perturb< S, E > perturb, 
    Order< S > order, 
    IsFinished< S, E > isFinished,
    Mutable< Double > initialTemperature, 
    Evaluate< S, V, E > eval,
    BiFunction< Double, Long, Double > schedule,    
    Random random ) {

    Accept< S, E > accept = MetropolisHastings.from( initialTemperature,
      eval,
      schedule, 
      random );
    return ReturnBest( perturb,
        accept,
        order,        
        isFinished );
  }
  ******/
  
  private def run[S,E]( initial: S,
    perturb: Perturb[S,E],
    accept: Accept[S,E], 
    isFinished: IsFinished[S,E],
    prefer: Option[Prefer[S,E]],
    env: E ) : (S,E) = {

    var incumbent = ( initial, env )    
    var best = incumbent
    
    var finished = isFinished( incumbent._1, incumbent._2 )   
    while( !finished._1 ) {
      
      var incoming = perturb( incumbent._1, incumbent._2 )
      incoming = accept( incoming._1, incumbent._1, incoming._2 )
      
      best = Prefer.choose( best._1, incoming._1, prefer, incoming._2 )
      finished = isFinished( incumbent._1, best._2 )
      incumbent = ( incumbent._1, finished._2 )
    }
    
    best
  }
  
}

//////////////////////////////////////////////////////////////////////

final class LocalSearch[S, E]( perturb: Perturb[S,E], 
  accept: Accept[S,E], 
  isFinished: IsFinished[S,E], 
  prefer: Option[Prefer[S,E]] ) extends IteratedPerturb[S,E] {

  /****
	trait Visitor< S, E > 
	// extends Copyable< Visitor< S, E > > 
	{

		public void accepted( S s, long iteration, long elapsedMillis, E e );
		public void best( S s, long iteration, long elapsedMillis, E e );
		
//		@Override
//		public Visitor< S, E > deepCopy();
	}
	******/
  
	override def apply( s: S, env: E ) : (S,E) =
		LocalSearch.run( s, perturb, accept, isFinished, prefer, env )			
}
************************/

// End ///////////////////////////////////////////////////////////////