package hyperion3.mutable

import org.mitlware.mutable._

package object isfinished {

  def maxIterations[S](maxIter: Long, numIterations: ReadOnly[Long])(x: S) : Boolean = {
    val iter = numIterations.get
    // numIterations.set( iter + 1 )
    iter == maxIter
  }
}

// End ///////////////////////////////////////////////////////////////
