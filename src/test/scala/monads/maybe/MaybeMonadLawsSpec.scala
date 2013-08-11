package monads.maybe

import monads.MonadLawsVerifier

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Provides flat specs for the Maybe type obeying monad laws.
  * 
  * Sample data and methods for the objects to be contained within
  * the Maybes are provided by the implementor by abstract method
  * definitions.
  */
class MaybeMonadLawsSpec extends MonadLawsVerifier with FlatSpec with ShouldMatchers {

  maybeShouldObeyMonadLaws(
    "Person data",
    Person.persons,
    { p: Person => p.mother },
    { p: Person => p.father })

  maybeShouldObeyMonadLaws(
    "safe math operations",
    safe.doubles,
    safe.safeSqrt _,
    safe.safeLog _)

  maybeShouldObeyMonadLaws(
    "looking up Numbers and Registrations by Name",
    lookup.names,
    lookup.lookupNumber _,
    lookup.lookupRegistration _)

  maybeShouldObeyMonadLaws(
    "looking up Registrations and TaxesOwed by Number",
    lookup.numbers,
    lookup.lookupRegistration _,
    lookup.lookupTaxOwed _)

  /** Runs FlatSpec tests against the provided test data and functions.
    *
    * @param testDataDescription a brief textual description of the test set
    * @param testItems a list of test items of type A to iterate over
    * @param f a test function from A to Maybe[B].
    * @param g a test function from B to Maybe[C].
    */
  def maybeShouldObeyMonadLaws[A, B, C](
    testDataDescription: String,
    testItems: Seq[A],
    f: Function1[A, Maybe[B]],
    g: Function1[B, Maybe[C]]): Unit = {

    val maybes = MaybeNot +: (testItems map { Just(_) })

    monadShouldObeyMonadLaws[A, B, C](
      "Maybe",
      MaybeMonad)(
      testDataDescription,
      testItems,
      maybes,
      f,
      g)

    maybeShouldFlatten(testDataDescription, testItems)
    maybeShouldFlatMapLikeMapAndFlatten(testDataDescription, maybes, f)
  }

  private def maybeShouldFlatten[A](
    testDataDescription: String,
    testItems: Seq[A]): Unit = {

    behavior of "Maybe.flatten with respect to " + testDataDescription
    it should "flatten a Maybe[Maybe[A]] into a Maybe[A] appropriately" in {
      testItems foreach { a =>
        { Just(Just(a)).flatten
        } should equal {
          Just(a)
        }
      }

      { Just(MaybeNot).flatten
      } should equal {
        MaybeNot
      }

      { MaybeNot.flatten
      } should equal {
        MaybeNot
      }
    }
  }

  private def maybeShouldFlatMapLikeMapAndFlatten[A, B](
    testDataDescription: String,
    maybes: Seq[Maybe[A]],
    f: Function1[A, Maybe[B]]): Unit = {

    def altFlatMap(m: Maybe[A], a: A => Maybe[B]): Maybe[B] = m.map(a).flatten

    behavior of "Maybe.flatMap with respect to " + testDataDescription
    it should "function equivalently to calling map and then flatten" in {
      maybes foreach { m =>
        { altFlatMap(m, f)
        } should equal {
          m flatMap f
        }
      }
    }
  }

}
