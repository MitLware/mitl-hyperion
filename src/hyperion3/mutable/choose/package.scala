package hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._

import scala.util.Random

package object choose {

//  def best[Entity,Value]( eval: Evaluate[Entity,Value])( 
//    implicit ev: DirectedValue[Value]) : Seq[Entity] => Option[Entity] =
//      ( l : Seq[Entity] ) => Option( l.maxBy( eval )( ev ) )

  def uniformRandom[Entity,Value](random: ReadWrite[Random]) : Seq[Entity] => Option[Entity] =
      ( l : Seq[Entity] ) => if( l.isEmpty ) Option.empty else Option( l( random.get.nextInt(l.size) ) )
}

// End ///////////////////////////////////////////////////////////////
