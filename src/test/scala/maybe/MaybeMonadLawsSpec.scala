package maybe

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Provides flat specs for the Maybe type obeying monad laws.
  * 
  * Sample data and methods for the objects to be contained within
  * the Maybes are provided by the implementor by abstract method
  * definitions.
  */
class MaybeMonadLawsSpec extends FlatSpec with ShouldMatchers {

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
    testItems: List[A],
    f: Function1[A, Maybe[B]],
    g: Function1[B, Maybe[C]]) {

    behavior of "Maybe monad with respect to " + testDataDescription

    it should "obey left unit monadic law" in {
      testItems foreach { a =>
        { Just(a) flatMap f
        } should equal {
          f(a)
        }
      }
    }

    val maybes = MaybeNot +: (testItems map { Just(_) })

    it should "obey right unit monadic law" in {
      maybes foreach { m =>
        { m flatMap { Just(_) }
        } should equal {
          m
        }
      }
    }

    it should "obey associativity monadic law" in {
      maybes foreach { m =>
        { m flatMap f flatMap g
        } should equal {
          m flatMap { a => f(a) flatMap g }
        }
      }
    }

    it should "flatten a Maybe[Maybe[_]] according to monadic laws" in {
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

    def altFlatMap(m: Maybe[A], a: A => Maybe[B]): Maybe[B] = m.map(a).flatten

    it should "flatMap equivalently to calling map and then flatten" in {
      maybes foreach { m =>
        { altFlatMap(m, f)
        } should equal {
          m flatMap f
        }
      }
    }
  }
}
