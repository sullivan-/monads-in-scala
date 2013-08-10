package monads

// TODO: reorg pacjages

import scala.language.higherKinds

abstract class Monad {

  type M[_]

  def unitFunction[A](a: A): M[A]

  def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B]

}

object Monad {

  import maybe._

  class MaybeMonad {

    type M[A] = Maybe[A]

    def unitFunction[A](a: A): M[A] = Just(a)

    def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B] = m flatMap f

  }

}
