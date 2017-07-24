package org.mitlware.hyperion3.immutable.perturb.permutation

import cats.data.State
import monocle.Lens

import org.mitlware.hyperion3.immutable._
import org.mitlware.solution.permutation.ArrayForm

///////////////////////////////////

object EvaluateFull {

///////////////////////////////////
  
/**
 * Swap two randomly-chosen elements. 
 * @book{Eiben:2003:IEC:954563,
 * 	 author = {Eiben, Agoston E. and Smith, J. E.},
 * 	 title = {Introduction to Evolutionary Computing},
 * year = {2003},
 * isbn = {3540401849},
 * publisher = {SpringerVerlag},
 * }
 */

case class RandomSwap[Env](rngLens: Lens[Env,RNG]) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = for {
      index1 <- RNG.nextInt(rngLens, x.size());
      index2 <- RNG.nextInt(rngLens, x.size())      
    } yield RandomSwap.perturb(x,index1,index2 )
}

///////////////////////////////////

object RandomSwap {
  
  def perturb(x: ArrayForm, index1: Int, index2: Int): ArrayForm = {
    require( index1 >= 0 && index1 < x.size() )
    require( index2 >= 0 && index2 < x.size() )    
    
    val result = x.toArray()      
    val temp = result(index1)
    result(index1) = result(index2) 
    result(index2) = temp
    new ArrayForm(result:_*)
  }
}

///////////////////////////////////

case class RandomReinsert[Env](rngLens: Lens[Env,RNG]) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = for {
      index1 <- RNG.nextInt(rngLens, x.size());
      index2 <- RNG.nextInt(rngLens, x.size())      
    } yield RandomReinsert.perturb(x,index1,index2 )
}

///////////////////////////////////

object RandomReinsert {

	def perturb(x: ArrayForm, i1: Int, i2: Int): ArrayForm = {
	  new ArrayForm(impl(x.toArray,i1,i2):_*)  
	}
  
  private def impl(array: Array[Int], index1: Int, index2: Int): Array[Int] = {
	  if( index1 == index2 )
	    array
	  else {	    

      val lo = math.min(index1,index2)
	    val hi = math.max(index1,index2)
	    
		  val newArray = new Array[Int](array.length)
		  newArray(lo) = array(hi)
		    
      var count = 0
      var i = 0
      while( count < array.length ) {
        if (i == lo) {
          count -= 1
        }
        else {
          newArray(i) = array(count)
          if (i == hi)
            count += 1
        }
        i += 1
        count += 1
      }
		  newArray		  
		}
	}
}

///////////////////////////////////

case class RandomFlip[Env](rngLens: Lens[Env,RNG]) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = for {
      index1 <- RNG.nextInt(rngLens, x.size());
      index2 <- RNG.nextInt(rngLens, x.size())      
    } yield RandomFlip.perturb(x,index1,index2 )
}

///////////////////////////////////

object RandomFlip {

	def perturb(permutation: ArrayForm, pa: Int, pb: Int): ArrayForm =
	  new ArrayForm( impl(permutation.toArray, pa, pb ):_* )
	
	def impl(permutation: Array[Int], pa: Int, pb: Int): Array[Int] = {
	  var a = pa; var b = pb
		if( a > b ){
			val temp = a
			a = b + 1
			b = temp - 1
		}				
		if(a == b)
			permutation.clone()
		else {
      val newPermutation = new Array[Int](permutation.length)
		  System.arraycopy(permutation, 0, newPermutation, 0, a)		
		  var count = a;
		  for( i <- b to a by -1 ){
			  newPermutation(count) = permutation(i)
			  count += 1
		  }
		  System.arraycopy(permutation, b + 1, newPermutation, b+1, permutation.length - b - 1)
		  newPermutation
    }
	}	
}
	
///////////////////////////////////

} // object EvaluateFull {

///////////////////////////////////

// End ///////////////////////////////////////////////////////////////
