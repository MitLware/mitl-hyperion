package org.mitlware.hyperion3.immutable

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens

///////////////////////////////////
case class EA[Env,Sol](
  iter: Lens[Env,Iter], 
  breed: Perturb[Env,List[Sol]],
  merge: Merge[Env,Sol], 
  isFinished: Condition[Env,List[Sol]]) extends Perturb[Env,List[Sol]] {

  override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = {
    def loop(s: List[Sol]): State[Env, List[Sol]] = for {
      _ <- State.modify[Env]( env => iter.modify ( _.inc )( env ) );
      incoming <- breed(s);
      merged <- merge(s,incoming);
      finished <- isFinished(merged);
      result <- if(finished) State.pure[Env,List[Sol]](merged) else loop(merged)
    } yield { result }
    
    loop(incumbent)
  }
}

object EA {

  // FIXME: replace Double with arbitrary ordered Value in this object

  // select a fixed number of survivors using truncation selection
  def selectTruncate[Env,Sol](
    mu: Int,
    eval: Lens[Env,Evaluate.Directional[Env,Sol,Double]]): Perturb[Env,List[Sol]] =
      new Perturb[Env,List[Sol]] {
        override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = for {
          env <- State.get[Env]
          val fitness =  eval.get(env)
          values <- incumbent.map(fitness.apply).sequence
          val combined = (incumbent,values).zipped map ((x,y) => (x,y))
          val sorted = combined.sortBy(x => fitness.direction.multiplier * x._2)
        } yield sorted.reverse.take(mu).map(_._1)
      }

  def pickTournament[Env,Sol](
    tournamentSize: Int,
    rngLens: Lens[Env,RNG] ): Perturb[Env,List[Sol]] =
    new Perturb[Env,List[Sol]] {
      override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = for {
          values <- incumbent.map(x => RNG.nextInt(rngLens)).sequence
          val combined = (incumbent,values).zipped map ((x,y) => (x,y))
          val sorted = combined.sortBy(x => x._2)          
        } yield sorted.take(tournamentSize).map(_._1)
    }

  def chooseBest[Env,Sol](
    eval: Lens[Env,Evaluate.Directional[Env,Sol,Double]])(xs: List[Sol]): State[Env,Sol] = for {
      env <- State.get[Env]
      val fitness =  eval.get(env)
      values <- xs.map(fitness.apply).sequence
      val combined = (xs,values).zipped map ((x,y) => (x,y))
      val sorted = combined.sortBy(x => fitness.direction.multiplier * x._2)
    } yield sorted.reverse.head._1

  // select a fixed number of survivors using tournament selection
  def selectTournament[Env,Sol](
    popsize: Int,
    tournamentSize: Int,
    rngLens: Lens[Env,RNG], 
    eval: Lens[Env,Evaluate.Directional[Env,Sol,Double]]): Perturb[Env,List[Sol]] =
      new Perturb[Env,List[Sol]] {
       override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = {
         val tournaments: List[State[Env,List[Sol]]] =
           for (i <- (1 to popsize).toList) yield pickTournament(tournamentSize, rngLens)(incumbent)
         val results: State[Env,State[Env,List[Sol]]] = tournaments.sequence.map(ts => ts.map(chooseBest(eval)).sequence)
         results.flatten
       }
      }

  // every parent produces a fixed number of children, parents are discarded
  def commaMutate[Env,Sol](
    childrenPerParent: Int,
    mutate: Perturb[Env,Sol]): Perturb[Env,List[Sol]] =
      new Perturb[Env,List[Sol]] {
        override def apply(parents: List[Sol]): State[Env,List[Sol]] =
          parents.flatMap( p => (1 to childrenPerParent).toList.map(_ => mutate(p)) ).sequence
      }

  // every parent produces a fixed number of children, parents are preserved
  def plusMutate[Env,Sol](
    childrenPerParent: Int,
    mutate: Perturb[Env,Sol]): Perturb[Env,List[Sol]] =
      new Perturb[Env,List[Sol]] {
        override def apply(parents: List[Sol]): State[Env,List[Sol]] =
          for {
            children <- commaMutate(childrenPerParent, mutate).apply(parents)
          } yield parents ++ children
      }

  // paired parents produce two children each, parents are discarded
  def geneticMutate[Env,Sol](
    mutate: Perturb[Env,Sol],
    crossover: Recombine2[Env,Sol]): Perturb[Env,List[Sol]] =
      new Perturb[Env,List[Sol]] {
        override def apply(parents: List[Sol]): State[Env, List[Sol]] = {
          val pairedParents: List[(Sol,Sol)] =
            ( parents.take(parents.length/2),
              parents.reverse.take(parents.length/2) ).zipped map ((x,y) => (x,y))
          // FIXME: unreadable due to major sequencing, maybe use a for loop?
          pairedParents.map( p =>
            List(crossover(p._1,p._2).flatMap(mutate.apply), crossover(p._2, p._1).flatMap(mutate.apply))
          ).map(_.sequence).sequence.map(_.flatten)
        }
      }
  

  // replace incumbent population with incoming population
  def replaceMerge[Env,Sol]: Merge[Env,Sol] = new Merge[Env,Sol] {
    override def apply(incumbent: List[Sol], incoming: List[Sol]) : State[Env,List[Sol]] =
      State.pure(incoming)
  }
    

  // (mu,lambda) Evolution Strategy
  def essentialsAlgorithm18[Env,Sol](
    muL: Lens[Env,Int],
    lambdaL: Lens[Env,Int],
    iter: Lens[Env,Iter],
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Double]],
    mutate: Perturb[Env,Sol],
    isFinished: Condition[Env,List[Sol]]): EA[Env,Sol] = {

      val breed: Perturb[Env,List[Sol]] = new Perturb[Env,List[Sol]] {
        override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = for {
          env <- State.get[Env]
          val mu = muL.get(env)
          val lambda = lambdaL.get(env)
          parents <- selectTruncate(mu, eval)(incumbent)
          children <- commaMutate(lambda/mu, mutate)(parents)
        } yield children
      }
     
      EA(iter, breed, replaceMerge, isFinished)
    }

  // (mu + lambda) Evolution Strategy
  def essentialsAlgorithm19[Env,Sol](
    muL: Lens[Env,Int],
    lambdaL: Lens[Env,Int],
    iter: Lens[Env,Iter],
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Double]],
    mutate: Perturb[Env,Sol],
    isFinished: Condition[Env,List[Sol]]): EA[Env,Sol] = {

      val breed: Perturb[Env,List[Sol]] = new Perturb[Env,List[Sol]] {
        override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = for {
          env <- State.get[Env]
          val mu = muL.get(env)
          val lambda = lambdaL.get(env)
          parents <- selectTruncate(mu, eval)(incumbent)
          children <- plusMutate(lambda/mu, mutate)(parents)
        } yield children
      }
     
      EA(iter, breed, replaceMerge, isFinished)
    }

  // Genetic Algorithm
  def essentialsAlgorithm20[Env,Sol](
    popsizeL: Lens[Env,Int],
    tournamentSizeL: Lens[Env,Int],
    rngLens: Lens[Env,RNG],
    iter: Lens[Env,Iter],
    eval: Lens[Env, Evaluate.Directional[Env,Sol,Double]],
    mutate: Perturb[Env,Sol],
    crossover: Recombine2[Env,Sol],
    isFinished: Condition[Env,List[Sol]]): EA[Env,Sol] = {

      val breed: Perturb[Env,List[Sol]] = new Perturb[Env,List[Sol]] {
        override def apply(incumbent: List[Sol]): State[Env,List[Sol]] = for {
          env <- State.get[Env]
          val popsize = popsizeL.get(env)
          val tournamentSize = tournamentSizeL.get(env)
          parents <- selectTournament(popsize, tournamentSize, rngLens, eval)(incumbent)
          children <- geneticMutate(mutate, crossover)(parents)
        } yield children
      }
     
      EA(iter, breed, replaceMerge, isFinished)
    }
  
}

// End ///////////////////////////////////////////////////////////////
