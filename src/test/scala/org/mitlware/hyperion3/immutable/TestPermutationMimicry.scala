package org.mitlware.hyperion3.immutable

import cats.data.State
import monocle.Lens

import org.mitlware.Diag

import org.mitlware.hyperion3.immutable._

import org.mitlware.support.lang.BadFormatException

import org.mitlware.support.lang.UnsupportedFormatException
import org.mitlware.support.math.Vec2

import org.mitlware.solution.permutation.ArrayForm
import org.mitlware.problem.tsp.TSP

import org.junit.Test

import java.io._
import java.nio.charset.Charset
import java.nio._
import java.nio.file._

import org.junit.Assert._ 

//////////////////////////////////////////////////////////////////////

/********************

case class ClosedInterval(min: Int, max: Int) {
  require( min <= max )  
}

object ClosedInterval {
  
  def union(a: ClosedInterval, b: ClosedInterval): ClosedInterval =
    ClosedInterval(math.min(a.min,b.min),math.max(a.max,b.max))
}

///////////////////////////////////

trait Permutation extends (Int => Int) {
   override def apply(i: Int): Int
   // require( i >= 0 )
   // ensuring result >= 0
   
   val preimage: ClosedInterval
   
   def toArrayForm: ArrayForm = {
     val array = (0 until preimage.max).toArray.map { i =>
       if( i < preimage.min ) i else apply( i )         
     }
     new ArrayForm(array:_*)
   }
}

///////////////////////////////////

object Permutation {
  
  def distinct[T](x: Iterable[T]): Boolean = 
    x.toSet.size == x.toSeq.length
  
  def isPermutation(a: Seq[Int]): Boolean = {
    a.forall { e => e >= 0 && e < a.length } && distinct( a )    
  }
  
  def isPermutation(m: Map[Int,Int]): Boolean = {
    distinct( m.keys ) && distinct( m.values ) &&
       m.keys.forall { _ >= 0 } &&
         m.values.forall { _ >= 0 }
  }
}

///////////////////////////////////

case class Sparse(impl: Map[Int,Int]) extends Permutation {
  require( Permutation.isPermutation( impl ) )
  
  override def apply(i: Int): Int = {
    require( i >= 0 )
    impl.getOrElse(i, i)
  }
  
  override val preimage: ClosedInterval = ClosedInterval(impl.keys.min, impl.keys.max)
  
  def update(deltas: Seq[(Int,Int)]): Sparse = {
    require( Permutation.distinct( deltas.unzip._1 ) )
    require( Permutation.distinct( deltas.unzip._2 ) )
      
    val rhs = Map() ++ deltas
    Sparse( impl.map { case (from,to) => (from,rhs.getOrElse( to, to ) } )
  }
  
  override def toString() = s"Sparse($impl)"
}

object Sparse {
  def apply(dense: Seq[Int]): Sparse = {
    require( Permutation.isPermutation(dense) )
    Sparse( Map() ++ (0 until dense.length).map { i => i -> dense(i) } )
  }
}

///////////////////////////////////

case class Dense(impl: Seq[Int]) extends Permutation {
  require( Permutation.isPermutation( impl ) )
  
  override def apply(i: Int): Int = {
    require( i >= 0 )
    if( i < impl.length ) impl(i) else i
  }
  
  override val preimage: ClosedInterval= ClosedInterval(0, impl.length - 1)
  
  override def toString() = s"Dense(${impl.mkString})"  
}

///////////////////////////////////

case class Transposition(index1: Int, index2: Int) extends Permutation {
  require( index1 >= 0 && index2 >= 0 )
    
  override def apply(i: Int): Int = {
    require( i >= 0 )    
    if( i == index1 ) index2 else { if( i == index2 ) index1 else i }
  }
  
  override val preimage: ClosedInterval = ClosedInterval(
    math.min(index1,index2),math.max(index1,index2))
    
  override def toString() = s"Transposition($index1,$index2)"    
}

///////////////////////////////////

case class Sequence(impl: List[Permutation]) extends Permutation {
  require(!impl.isEmpty )
  
  override def apply(i: Int): Int = {
    require( i >= 0 )    
    val composed = impl.map { _.asInstanceOf[Int => Int] 
      }.reduceLeft( (acc,current) => acc.andThen(current) )
    composed(i)
  }

  override val preimage: ClosedInterval = 
    impl.map { _.preimage }.reduce { ClosedInterval.union(_,_) }
    
  override def toString() = s"Sequence(${impl.mkString(",")})"    
}

object Sequence {
  
  def apply(hd: Permutation, andThen: Permutation): Sequence = (hd,andThen) match {
    case (Sequence(lhs),Sequence(rhs)) => Sequence(lhs ++ rhs)
    case (Sequence(lhs),_) => Sequence(lhs :+ andThen)
    case (_,Sequence(rhs)) => Sequence(hd+: rhs)
    case _ => Sequence(List(hd,andThen))    
  }
}
********************/

//////////////////////////////////////////////////////////////////////

class TestPermutationMimicry {

	@Test
	def test: Unit = {
    val InstanceSize = 6
    val targetImpl = (0 until InstanceSize).toArray
	  val target = new ArrayForm( targetImpl:_* )
    val source = new ArrayForm( targetImpl.reverse:_* )
	  
    val Seed = 0x15d6940772fL
	  val (solution,solutionFitness) = PermutationMimicry.search(source,target,numIter=1000, seed = Seed)
	  assertEquals( target, solution )	  
	  assertEquals( 0, solutionFitness )
	}
}

// End ///////////////////////////////////////////////////////////////
