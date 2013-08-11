package maybe

import monads.Monad

object MaybeMonad extends Monad {

  type M[A] = Maybe[A]

  def unitFunction[A](a: A): M[A] = Just(a)

  def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B] = m flatMap f
}
