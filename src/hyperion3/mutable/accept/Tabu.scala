package hyperion3.mutable.accept

import org.mitlware._
import org.mitlware.mutable._

import hyperion3.mutable._

import org.apache.commons.collections15.buffer.CircularFifoBuffer

//////////////////////////////////////////////////////////////////////

final object Tabu {

  def strict[S,Feature]( prefer: Prefer[S],
    featureFn: ( S, S ) => Set[Feature],      
    tabuList: ReadWrite[CircularFifoBuffer[Set[Feature]]] )
  ( incumbent: S, incoming: S ) : S = {
    
    def intersects(x: Set[Feature]) = new java.util.function.Predicate[Set[Feature]] {
      override def test(s: Set[Feature]) : Boolean = !(s & x).isEmpty       
    }
    
    if( prefer.prefer( incumbent, incoming ) != Preference.PREFER_RIGHT )
      incumbent
    else {
      val newFeatures = featureFn( incumbent, incoming )
      val tl = tabuList.get
      if( tl.stream().anyMatch( intersects(newFeatures) ) ) 
        incumbent
      else {
        tl.add( newFeatures )
        incoming
      }
    }
  }
}

// End ///////////////////////////////////////////////////////////////
