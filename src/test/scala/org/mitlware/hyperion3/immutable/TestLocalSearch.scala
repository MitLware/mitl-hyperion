package org.mitlware.hyperion3.immutable



/*************************************************

import org.mitlware._
import org.mitlware.immutable._
import hyperion3.immutable._
import hyperion3.immutable.isfinished._
import statelet.bitvector._

import java.util.Comparator

import org.junit._
import org.junit.Assert._

//////////////////////////////////////////////////////////////////////

/**********
final class TestLocalSearch {

	@Test
	def test() : Unit = {
      
//		assertEquals( 0.0, schwefelFn( CommonsMathRealVector( Array( 420.9687 ) ) ), 0.001 )
	}
}
**********/

/*********

final class CountOnes[Env]
extends Evaluate[BitVector, Max[Double],Env] {
  
    override def apply(x: BitVector, env: Env) : (Max[Double],Env) = {
        
        var count = 0
        for( i <- 0 until x.length() )
            if( x.get( i ) )
                count += 1

        val cmp = new Comparator[Double]() {
            def compare(a: Double, b: Double) = ( a - b ).toInt
        }
                
        (Max[Double]( count / x.length().toDouble, cmp ), env )
    }
}
*********/

//////////////////////////////////////////////////////////////////////

final class BitFlip[Env](index: Int) extends Perturb[BitVector,Env] {

  @Override
  def apply( x: BitVector, env: Env ) : (BitVector,Env) = {
      val copy = x.clone()
      copy.flip(index)
      (copy,env)
  }   
}

//////////////////////////////////////////////////////////////////////

/**********

final class UniformMutation[Env] extends Perturb[BitVector,Env] {

  @Override
  def apply( x: BitVector, env: Env ) : (BitVector,Env) = applyCoarseGrained( x,e )   
  
//  private Tuple2< TabuEnv< BitString >, BitString >  
//  applyFineGrained( Trace< TabuEnv< BitString >, BitString > t) {
//    final int index = RNG.get().nextInt( t.last()._2.size() );
//    BitFlip op = new BitFlip( index );
//    return op.apply( t );
//  }
  
  private def applyCoarseGrained( Trace< TabuEnv< BitString >, BitString > t ) {
    BitString neighbour = new BitString( t.last()._2 ); 
    final int index = RNG.get().nextInt( t.last()._2.size() );    
    neighbour.flip( index );
    return new Tuple2< TabuEnv< BitString >, BitString >( Envs.TabuEnv.create( this ), neighbour );
  }
}
**********/

//////////////////////////////////////////////////////////////////////

/******

final class BitFlipLocality extends Locality[BitVector,Void] {

  public List<Tuple2<TabuEnv<BitString>, BitString>> apply(
      Trace<TabuEnv<BitString>, BitString> t ) {
    
    List<Tuple2<TabuEnv<BitString>, BitString>> result = new ArrayList<Tuple2<TabuEnv<BitString>, BitString>>();
    BitString s = t.last()._2;
    for( int i=0; i<s.size(); ++i ) {
      BitFlip op = new BitFlip( i );
      result.add( op.apply( t ) );
    }
    
    return result;
  }
}
******/
//////////////////////////////////////////////////////////////////////

object TestILS {

  /****
    def run( random: scala.util.Random  ) : Unit = {

      val bitstringLength = 5
      val MaxIterations = 10L // 512
      
      var env = Workspace()

      val initialState = new BitVector( bitstringLength, random.self );
      val eval : Evaluate[BitVector,Max[Double],Workspace]= new CountOnes[Workspace]
    // Op< TabuEnv< BitString >, BitString > mutate = new UniformMutation();
      val accept = Accept.improving( Prefer.from( eval ) )
      
      val perturb = new BitFlip[Workspace](0)
    
    // val locality = new BitFlipLocality()
    // val select = new TruncationSelect< TabuEnv< BitString >, BitString, Max< Double > >( eval )   

    val (numIterations,env2) = Mutable[Long](0L,env)
    val isFinished = finished.MaxIterations[BitVector](MaxIterations,numIterations)
      
    val ls = LocalSearch.returnLast( perturb, accept, isFinished )
    
    val initial = new BitVector( 10 )
    
    println( ls( initial, env2 ) )
  }

  /////////////////////////////////
    
  def main(args: Array[String] ) : Unit = {

    val randomSeed = 0x12345678L 
    val random = new scala.util.Random( randomSeed )
    run( random );
  }
*****/
}

*************************************************/

// End ///////////////////////////////////////////////////////////////

