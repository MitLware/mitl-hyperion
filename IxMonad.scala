package org.mitlware.hyperion3.immutable

// From: https://gist.github.com/pthariensflame/5054294

object IState extends IStateMonadFuncs with IStateFuncs {
  def apply[I, O, A](run: I => (A, O)): IState[I, O, A] = new IState[I, O, A](run)
}
final class IState[-I, +O, +A](val run: I => (A, O)) extends IStateMonadOps[I, O, A] {
  def eval(i: I): A = this.run(i)._1
  def exec(i: I): O = this.run(i)._2
}

private[indexedState] sealed trait IStateMonadFuncs { this: IState.type =>
  // unit
  def point[S, A](a: A): IState[S, S, A] = IState { s => (a, s) }
}
private[indexedState] sealed trait IStateMonadOps[-I, +O, +A] { this: IState[I, O, A] =>
  // map
  def map[B](f: A => B): IState[I, O, B] = IState { i =>
    val (a, o) = this.run(i)
    (f(a), o)
  }

  // join
  def flatten[E, B](implicit ev: A <:< IState[O, E, B]): IState[I, E, B] = IState { i =>
    val (n, o) = this.run(i)
    ev(n).run(o)
  }

  // bind
  def flatMap[E, B](f: A => IState[O, E, B]): IState[I, E, B] = IState { i =>
    val (n, o) = this.run(i)
    f(n).run(o)
  }
}

private[indexedState] sealed trait IStateFuncs { this: IState.type =>
  def get[S]: IState[S, S, S] = IState { s => (s, s) }

  def put[O](o: O): IState[Any, O, Unit] = IState { _ => ((), o) }

  def modify[I, O](f: I => O): IState[I, O, Unit] = IState { i => ((), f(i)) }
}

///////////////////////////////////

object example {

  def someIntToCharFunction(x: Int): Char = ???

  /**
   * Performs `someIntToCharFunction` on the input state, returning the old state.
   */
  val myIStateComputation: IState[Int, Char, Int] = for {
    original <- IState.get[Int]
    _        <- IState.modify[Int, Char](someIntToCharFunction)
  } yield original
}

/////////////////////////////////// // package indexedState {



// End ///////////////////////////////////////////////////////////////
