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

// End ///////////////////////////////////////////////////////////////
