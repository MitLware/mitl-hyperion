package org.mitlware.hyperion3.immutable

import cats.data.State
import cats.Semigroup
import monocle._

///////////////////////////////////

/*******
case class EA[Env,Sol](
  iter: Lens[Env,Iter], 
  order: Lens[Env, Order[Env,Sol]],  
  perturb: Perturb[Env,Sol],
  accept: Accept[Env,Sol], 
  isFinished: Condition[Env,Sol]) extends Perturb[Env,List[Sol]] {

  override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = {
    def loop(s: List[Sol]): State[Env, List[Sol]] = for {
      _ <- State.modify[Env]( env => iter.modify ( _.inc )( env ) );
      incoming <- perturb(s);
      env <- State.get[Env];      
      // best <- order.get(env).apply(bestSoFar,incoming);
      accepted <- accept(s,incoming);
      finished <- isFinished(accepted);
      result <- if(finished) State.pure[Env,Sol](accepted) else loop(accepted)
    } yield { result }
    
    loop(incumbent)
  }
}  
*******/

/**********
trait CrossoverView[T] {
  def getCrossover : CrossoverImpl[T]
  def setCrossover( order : CrossoverImpl[T] ) : this.type
}

trait CrossoverImpl[T] {
  def crossover( x : T, y : T ) : T
}

object Crossover {
  def apply[T,Env <: CrossoverView[T]]( x : T, y : T )  : State[Env,T] = State { s => (s,s.getCrossover.crossover(x,y)) }
}


trait MutationView[T] {
  def getMutation : MutationImpl[T]
  def setMutation( order : MutationImpl[T] ) : this.type
}

trait MutationImpl[T] {
  def mutate( x : T ) : T
}

object Mutation {
  def mutate[T,Env <: MutationView[T]]( x : T )  : State[Env,T] = State { s => (s,s.getMutation.mutate(x)) }
}

// TODO: replace with Lenses to support multiple populations
trait PopulationView[T] {
  def getPopulation : List[T]
  def setPopulation( population : List[T] ) : this.type
}

trait PopulationImpl[T] {
  def members : List[T]
}

object Population {
  
  def size[Ind,Env <: PopulationView[Ind]] : State[Env,Int] = State(s=>(s,s.getPopulation.size))
  
  def members[Ind,Env <: PopulationView[Ind]] : State[Env,List[Ind]] = State(s=>(s,s.getPopulation))
  
  def replaceMembers[Ind,Env <: PopulationView[Ind]]( members : List[Ind] ) : State[Env,List[Ind]] = State(s=>(s.setPopulation(members),members))
  
  def uniformSelect[Ind,Env <: PopulationView[Ind] with RNGView] : State[Env,Ind] = for {
    pop    <- members[Ind,Env]
    sample <- RNG.nextInt(pop.size)
  } yield pop(sample)
  
  def best[Ind,Env <: PopulationView[Ind] with OrderView[Ind]] : State[Env,Ind] = {
    def bestOf( members : List[Ind] ) : State[Env,Ind] = members match {
      case Nil                => throw new Exception("Empty population")
      case (first :: Nil)     => State(s=>(s,first))
      case (first :: members) => for {
        rest <- bestOf( members )
        best <- Order.better(first, rest)
      } yield best
    }
    
    for { 
      pop   <- members[Ind,Env]
      best <- bestOf(pop)
    } yield best
  }
  
  def reproduce[Ind, Env <: 
    PopulationView[Ind] with 
    MutationView[Ind]   with
    CrossoverView[Ind]  with
    RNGView] : State[Env,List[Ind]] = {
    
    def addChild(current : List[Ind]) : State[Env,List[Ind]] = for {
        parent1 <- uniformSelect[Ind,Env]
        parent2 <- uniformSelect[Ind,Env]
        child1  <- Crossover(parent1,parent2)
        child2  <- Crossover(parent1,parent2)
        mchild1 <- Mutation.mutate(child1)
        mchild2 <- Mutation.mutate(child2)
    } yield (mchild1 :: mchild2 :: current)
    
    def fill(current : List[Ind]) : State[Env,List[Ind]] = for {
      s <- size[Ind,Env]
      expanded <- addChild(current)
      result <- if( s > current.length ) fill(expanded) else finishWith[Env,List[Ind]](current)
    } yield result.take(s)
    
    for {
      newPop <- fill(Nil)
      _ <- replaceMembers[Ind,Env](newPop)
    } yield newPop
  }
  
}

**********/

/**********

object EA {
  
//Env <: 
//    PopulationView[Sol] with 
//    OrderView[Sol]      with 
//    MutationView[Sol]   with
//    CrossoverView[Sol]  with
//    BestSoFarView[Sol]  with
//    RNGView
    
  def search[Env,Sol](isFinished: Condition[Sol,Env]) : State[Env,Sol] = for {
      pop       <- Population.reproduce[Sol,Env]
      popBest   <- Population.best[Sol,Env]
      bestSoFar <- BestSoFar.compare(popBest)
      finished  <- isFinished(bestSoFar)
      result    <- if (finished) State((s:Env)=>(s,bestSoFar)) else search(isFinished)
  } yield result
}
**********/

object BestSoFar {
//  def compare[Env,T](t: T, 
//    bestSoFar: Lens[Env,T], 
//    order: Lens[Env,Order[Env,T]]): State[Env,T] = State[Env,T] { env =>
//      
//      order.get(env).apply(t, bestSoFar.get(env))       
//  }
    
//    val b = s.getBestSoFar
//    val ord = s.getOrder
//    val better = ord.better(t, b)
//    (s.setBestSoFar(better),better)
//  }
//  def get[T, Env <: BestSoFarView[T]] : State[Env,T] = State { s => (s,s.getBestSoFar) }
}

/*********
case class EA[Env,Sol](
  iter: Lens[Env,Iter], 
  order: Lens[Env, Order[Env,Sol]],  
  breed: Perturb[Env,List[Sol]],
  accept: Accept[Env,Sol], 
  isFinished: Condition[Env,List[Sol]]) extends Perturb[Env,List[Sol]] {

  override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = {
    def loop(s: List[Sol], bestSoFar: Sol): State[Env, List[Sol]] = for {
      _ <- State.modify[Env]( env => iter.modify ( _.inc )( env ) );
      incoming <- breed(s);
      env <- State.get[Env];      
      best <- order.get(env).apply(bestSoFar,incoming);
      accepted <- accept(s,incoming);
      finished <- isFinished(accepted);
      result <- if(finished) State.pure[Env,Sol](best) else loop(accepted,best)
    } yield { result }
    
    loop(incumbent,incumbent)
  }
}
*********/
// End ///////////////////////////////////////////////////////////////
