package monads

import scala.language.higherKinds

abstract class Monad {

  type M[_]

  def unitFunction[A](a: A): M[A]

  def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B]

}
