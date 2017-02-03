package monads

import scala.language.higherKinds

trait Monad {

  type M[_]

  def unitFunction[A](a: A): M[A]

  def bindingOperation[A, B](m: M[A], f: (A) => M[B]): M[B]

  implicit class KleisliComposition[B, C](f: Function[B, M[C]]) {
    def kleisliCompose[A](g: (A) => M[B]): (A) => M[C] = {
      a: A => bindingOperation(g(a), f)
    }
  }

}
