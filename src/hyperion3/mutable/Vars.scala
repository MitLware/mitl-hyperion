package hyperion3.mutable

///////////////////////////////////

trait ReadOnly[T] {
  implicit def get : T  
}

trait ReadWrite[T] extends ReadOnly[T] {
  def set(x: T) : Unit
}

trait VariableFactory {

  def readOnly[T](x: T) : ReadOnly[T]
  def readWrite[T](x: T) : ReadWrite[T]  
}

///////////////////////////////////

object GlobalVars {
  
///////////////////////////////////
  
  object Factory extends VariableFactory {
    override def readOnly[T](x: T) : ReadOnly[T] = Immutable(x)
    override def readWrite[T](x: T) : ReadWrite[T] = Mutable(x)
  }
  
///////////////////////////////////  

case class Immutable[T](private var value: T, name: Option[String]) extends ReadOnly[T] {

  override def get : T = value
  
  // override def toString = "name:" + name  
}

object Immutable {

  def apply[T](x: T) : Immutable[T] = Immutable(x,Option.empty)
  def apply[T](x: T,name: String ) : Immutable[T] = Immutable(x,Option(name))  
}

///////////////////////////////////

case class Mutable[T](private var value: T,name: Option[String]) extends ReadWrite[T] {

  override def get : T = value
  def set(x: T) : Unit = value = x  
}

object Mutable {

  def apply[T](x: T) : Mutable[T] = Mutable(x,Option.empty)
  def apply[T](x: T,name: String ) : Mutable[T] = Mutable(x,Option(name))  
}

///////////////////////////////////

} // object GlobalVars {

///////////////////////////////////

object WorkspaceVars {
  
///////////////////////////////////
  
  class Factory( implicit env: Workspace ) extends VariableFactory {
    override def readOnly[T](x: T) : ReadOnly[T] = Immutable(x)
    override def readWrite[T](x: T) : ReadWrite[T] = Mutable(x)
  }

///////////////////////////////////  

case class Immutable[T](private val key: Workspace.ReadKey[T],
  implicit val env: Workspace) extends ReadOnly[T] {

    override def get : T = env.get( key )
}

object Immutable {
  
  def apply[T](initial: T)( implicit env: Workspace) : Immutable[T] = {
    val key = env.putReadOnly(initial)
    Immutable(key,env)
  }
}

///////////////////////////////////

case class Mutable[T](val key: Workspace.ReadWriteKey[T],
  implicit val env: Workspace) extends ReadWrite[T] {

  override def get : T = env.get( key )
  override def set(x: T) : Unit = env.set( key, x )
}

object Mutable {
  
  def apply[T](initial: T)(implicit env: Workspace) : Mutable[T] = {
    val key = env.putReadWrite(initial)
    Mutable(key,env)
  }  
}

///////////////////////////////////

} // object SharedVars {

// End ///////////////////////////////////////////////////////////////
