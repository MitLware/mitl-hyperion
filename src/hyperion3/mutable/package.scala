package hyperion3

import org.mitlware._
import org.mitlware.mutable._

package object mutable {

  implicit def liftAccept[S](f: Function2[S,S,S]) : Accept[S] = new Accept[S] {
    override def apply(incumbent: S , incoming: S ) : S = f(incumbent, incoming )    
  }

  implicit def liftEvaluate[S,V]( f: S => V ) : Evaluate[S,V] = new Evaluate[S,V] {
    override def apply(x: S) : V = f(x)    
  }
  
  implicit def liftIsFinished[S]( f: S => Boolean ) : IsFinished[S] = new IsFinished[S] {
    override def apply(x: S) : java.lang.Boolean = f(x)    
  }

  implicit def liftLocality[S]( f: S => java.util.List[S] ) : Locality[S] = new Locality[S] {
    override def apply(x: S) : java.util.List[S] = f(x)    
  }
  
  implicit def liftMove[S]( f: ( S, java.util.stream.Stream[S] ) => java.util.Optional[S] ) : Move[S] = new Move[S] {
    override def apply(incumbent: S,locality: java.util.stream.Stream[S]) : java.util.Optional[S] = f(incumbent,locality )    
  }
  
  implicit def liftPerturb[S]( f: S => S ) : Perturb[S] = new Perturb[S] {
    override def apply(incumbent: S) : S = f(incumbent )    
  }
  
  implicit def liftPrefer[S]( f: Function2[S,S,S] ) : Prefer[S] = new Prefer[S] {
    override def prefer(a: S, b: S) : S = f(a, b)    
  }

}

// End ///////////////////////////////////////////////////////////////
