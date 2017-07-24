package org.mitlware.hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._
import org.mitlware.hyperion3.mutable._
import org.mitlware.solution.bitvector._
import java.lang.{Double=>JDouble}
import org.junit._
import org.junit.Assert._
import scala.util.Random

//////////////////////////////////////////////////////////////////////

object Onemax {

  private def countOnesImpl(x: BitVector ) : JDouble = x.cardinality() / x.length().toDouble
  
  val countOnes : Evaluate.Directional[BitVector,JDouble] = 
    Evaluate.Directional.maximizing( countOnesImpl _ )
    
  def uniformMutation(random: ReadWrite[Random]) = new Perturb[BitVector] {
    override def apply(x: BitVector) : BitVector = {
      val result = x.clone()
// jeep.lang.Diag.println( "before:" + result )      
      result.flip( random.get.nextInt( x.length() ) )
// jeep.lang.Diag.println( "after:" + result )      
      result
    }
  }

  def uniformMutationInstrumented(lastPerturbedIndices: ReadWrite[Set[Int]],random: ReadWrite[Random])(x: BitVector) : BitVector = {
    val result = x.clone()
    val index = random.get.nextInt( x.length() )
    lastPerturbedIndices.set(Set(index))
    result.flip( index )
    result
  }
  
  def flipBit( x: BitVector, index: Int ) : BitVector = {
    val copy = x.clone()
    copy.flip(index)
    copy
  }   
  
  def bitflipLocality(x: BitVector ) : java.util.List[BitVector] = {
    import scala.collection.JavaConversions._
    for( i <- Seq.range( 0, x.length ) ) yield flipBit( x, i )
  }

}

// End ///////////////////////////////////////////////////////////////
