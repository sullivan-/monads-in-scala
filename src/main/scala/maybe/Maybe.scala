package maybe

sealed trait Maybe[+A] {

  // >>=
  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  // >>
  def map[B](f: A => B): Maybe[B] = flatMap { a => Just(f(a)) }
}

case class Just[+A](a: A) extends Maybe[A] {
  override def flatMap[B](f: A => Maybe[B]) = f(a)
}

// Nothing in the Haskel example
case object MaybeNot extends Maybe[Nothing] {
  override def flatMap[B](f: Nothing => Maybe[B]) = MaybeNot
}
