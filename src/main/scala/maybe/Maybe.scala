package maybe

object Maybe {

  // return
  def apply[A](a: A) = Just(a)
}

trait Maybe[+A] {

  // >>=
  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  // >>
  def map[B](f: A => B): Maybe[B]
}

case class Just[+A](a: A) extends Maybe[A] {
  override def flatMap[B](f: A => Maybe[B]) = f(a)
  override def map[B](f: A => B) = flatMap { aa => Maybe(f(aa)) }
}

// Nothing in the Haskel example
case object MaybeNot extends Maybe[Nothing] {
  override def flatMap[B](f: Nothing => Maybe[B]) = MaybeNot
  override def map[B](f: Nothing => B) = MaybeNot
}
