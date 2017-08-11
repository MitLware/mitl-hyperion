package org.mitlware.hyperion3.immutable



/***********

import org.mitlware.immutable._

object Workspace {

  final case class ReadKey[A](index: Long)
  final case class ReadWriteKey[A](index: Long)
  
  def apply() : Workspace = new Workspace(Map[Long, Any]())
}

///////////////////////////////////////////////

final case class Workspace(private val map : Map[Long, Any]) {
  
  import Workspace._
  
  def get[A](key: ReadKey[A]) : A = map(key.index).asInstanceOf[A]
  def get[A](key: ReadWriteKey[A]) : A = map(key.index).asInstanceOf[A]
  
  def set[A](key: ReadWriteKey[A], a: A) : Workspace = Workspace(map + ( key.index -> a ) )  

  def putReadOnly[A](a: A) : ( ReadKey[A], Workspace) = {
    val index = map.size.toLong
    (new ReadKey( index ), Workspace( map + ( index -> a ) ) )
  }
  
  def putReadWrite[A](a: A) : (ReadWriteKey[A],Workspace) = {
    val index = map.size.toLong
    (new ReadWriteKey( index ), Workspace( map + ( index -> a ) ) )
  }
}
***********/

// End ///////////////////////////////////////////////////////////////
