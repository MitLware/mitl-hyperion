package hyperion3.mutable

import org.mitlware.mutable._

//////////////////////////////////////////////////////////////////////

object Workspace {

  final case class ReadKey[A](index: Long)
  final case class ReadWriteKey[A](index: Long)
  
  def apply() : Workspace = new Workspace(scala.collection.mutable.Map[Long, Any]())
}

///////////////////////////////////////////////

final case class Workspace(private val map : scala.collection.mutable.Map[Long, Any]) {
  
  import Workspace._
  
  def numEntries : Int = map.size
  
  def get[A](key: ReadKey[A]) : A = map(key.index).asInstanceOf[A]
  def get[A](key: ReadWriteKey[A]) : A = map(key.index).asInstanceOf[A]
  
  def set[A](key: ReadWriteKey[A], a: A) : Unit = map += ( key.index -> a )  

  import WorkspaceVars._
  
  def putReadOnly[A](a: A) : ReadKey[A] = {
    val index = map.size.toLong
    map += ( index -> a )
    new ReadKey( index ) 
  }
  
  def putReadWrite[A](a: A) : ReadWriteKey[A] = {
    val index = map.size.toLong
    map += ( index -> a )
    new ReadWriteKey( index )
  }
}

//////////////////////////////////////////////////////////////////////


/*******************************************

final object Workspace {

  private var firstUnusedIndex = 0L
  private val map = new collection.mutable.HashMap[Long, Any].empty
  
  ///////////////////////////////
  
  case class ReadKey[A](val index: Long)
  final class ReadWriteKey[A](override val index: Long) extends ReadKey(index)  

  ///////////////////////////////
  
  def get[A]( key: ReadKey[A] ) : A = map(key.index).asInstanceOf[A]
  def set[A]( key: ReadWriteKey[A], a: A ) : Unit = map.put(key.index,a)  

  def put[A](a: A, permission: CanRead[A]) : ReadKey[A] = {
    map.put( firstUnusedIndex, a )
    firstUnusedIndex += 1
    new ReadKey( firstUnusedIndex )
  }
  
  def put[A](a: A, permission: CanReadWrite[A]) : ReadWriteKey[A] = {
    map.put( firstUnusedIndex, a )
    firstUnusedIndex += 1
    new ReadWriteKey( firstUnusedIndex )
  }
  
  /*
   * new SA(Ticket<Temp> x);
   * 
   */
  
  
  trait CanRead[T]
  trait CanReadWrite[T]

  def main(args: Array[String]) : Unit = {
    
    case class MaxIterations[S](maxIter: Int) extends IsFinished[S] with CanReadWrite[Long] {
      
      val key = Workspace.put( 0L, this )
      
      def apply(x: S) : Boolean = {
        
        var numIterations : Long = Workspace.get( key )
        numIterations += 1
        
        Workspace.set( key, numIterations )
        
        numIterations == maxIter
      }

    }
    
    val xx = Workspace    
  }
}
*******************************************/

// End ///////////////////////////////////////////////////////////////
