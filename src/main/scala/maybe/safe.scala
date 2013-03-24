package maybe

object safe {

  /** A series of doubles useful for testing safe methods. */
  val doubles = List[Double](
    -2, // log, sqrt produce NaN
    -1, // log, sqrt produce NaN
    0, // log produces -Infinity, sqrt produces 0
    0.5, // log produces negative
    1, // log produces 0
    2) // keeping things positive

  def safeSqrt(d: Double): Maybe[Double] =
    if (d >= 0) Just(scala.math.sqrt(d)) else MaybeNot

  def safeLog(d: Double): Maybe[Double] =
    if (d > 0) Just(scala.math.log(d)) else MaybeNot

  // according to first definition of safeLogSqrt provided in wiki
  def safeLogSqrt0(d: Double): Maybe[Double] =
    safeSqrt(d) match {
      case Just(d) => safeLog(d)
      case MaybeNot => MaybeNot
    }

  // according to second definition of safeLogSqrt provided in wiki  
  def safeLogSqrt1(d: Double): Maybe[Double] =
    Just(d) flatMap safeSqrt flatMap safeLog

  // a simplification of safeLogSqrt1 deriving from "left unit" monadic law
  def safeLogSqrt2(d: Double): Maybe[Double] =
    safeSqrt(d) flatMap safeLog

  // chain sqrt and log in a for loop
  def safeLogSqrt3(d: Double): Maybe[Double] =
    for (s <- safeSqrt(d); l <- safeLog(s)) yield l

  // sqrt gets called first, then log
  val unsafeLogSqrt: (Double) => Double =
    scala.math.log _ compose scala.math.sqrt _

  def kleisliCompose[A, B, C](
    f: (B) => Maybe[C],
    g: (A) => Maybe[B]):
  (A) => Maybe[C] = {
    a: A =>
    for (b <- g(a); c <- f(b)) yield c
  }

  def safeLogSqrt4(d: Double): Maybe[Double] =
    kleisliCompose(safeLog _, safeSqrt _)(d)

  implicit class MaybeFunction[B, C](f: Function[B, Maybe[C]]) {
    def kleisliCompose[A](g: (A) => Maybe[B]): (A) => Maybe[C] = {
      a: A =>
      for (b <- g(a); c <- f(b)) yield c
    }
  }

  def safeLogSqrt5(d: Double): Maybe[Double] =
    (safeLog _ kleisliCompose safeSqrt _)(d)
}
