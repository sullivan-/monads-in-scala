package monads.lisst

import monads.Monad

object LisstMonad extends Monad {

  type M[A] = Lisst[A]

  def unitFunction[A](a: A): M[A] = Lisst(a)

  def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B] = m flatMap f

}
