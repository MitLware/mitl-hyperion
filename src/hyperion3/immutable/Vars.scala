package hyperion3.immutable

/***************************

case class Immutable[T](private val key: Workspace.ReadKey[T]) {

  def get(env: Workspace) : T = env.get( key )
}

object Immutable {
  
  def apply[T](initial: T, env: Workspace) : (Immutable[T],Workspace) = {
    val (key,newEnv) = env.putReadOnly(initial)
    (Immutable(key),newEnv)
  }  
}

///////////////////////////////////

case class Mutable[T](val key: Workspace.ReadWriteKey[T]) {

  def get(env: Workspace) : T = env.get( key )
  def set(x: T, env: Workspace) : Workspace = env.set( key, x )  
}

object Mutable {
  
  def apply[T](initial: T, env: Workspace) : (Mutable[T],Workspace) = {
    val (key,newEnv) = env.putReadWrite(initial)
    (Mutable(key),newEnv)
  }  
}
***************************/

// End ///////////////////////////////////////////////////////////////
