package monads.lisst

import monads.MonadLawsVerifier

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class LisstMonadLawsSpec extends FlatSpec with MonadLawsVerifier with Matchers {

  import testdata._

  private val testInts = Seq(0, 1, 2)
  private val testLissts = Seq(Lisst[Int](), Lisst(0), Lisst(0, 1, 2))

  private val fgPairs = for (
    f <- Seq(toZeroStrings _, toOneString _ , toThreeStrings _);
    g <- Seq(toZeroDoubles _, toOneDouble _ , toThreeDoubles _))
  yield (f, g)

  fgPairs.zipWithIndex.foreach {
    case ((f, g), i) =>
      lisstShouldObeyMonadLaws(
        "toString and toDouble conversions, combination number " + (i + 1),
        testInts,
        testLissts,
        f,
        g)
  }

  private def lisstShouldObeyMonadLaws[A, B, C](
    testDataDescription: String,
    testAs: Seq[A],
    testLissts: Seq[Lisst[A]],
    f: Function1[A, Lisst[B]],
    g: Function1[B, Lisst[C]]): Unit =
    monadShouldObeyMonadLaws[A, B, C](
      "Lisst",
      LisstMonad)(
      testDataDescription,
      testAs,
      testLissts,
      f,
      g)

}
