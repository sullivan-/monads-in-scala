package maybe

import org.scalatest.FlatSpec

class MaybeMonadLawsSpecPersonSpec extends FlatSpec {

  behavior of "Maybe monad"
  it should "obey monad laws with respect to the person data" in {
    Person.persons foreach { p =>

      // left unit
      assert((Just(p) flatMap { _.mother }) == p.mother)
    }

    val maybes = MaybeNot +: (Person.persons map { Just(_) })
    maybes foreach { m =>

      // right unit
      assert((m flatMap { Just(_) }) == m)

      // associativity
      assert(
        (m flatMap { _.mother } flatMap { _.father }) ==
        (m flatMap { _.mother flatMap { _.father } }))
    }
  }

  behavior of "Maybe.flatten"
  it should "flatten a Maybe[Maybe[_]] according to monadic laws" in {

    // will not compile:
    // Just(1).flatten
    // compiler error is:
    // Cannot prove that maybe.Maybe[Int] <:< maybe.Maybe[maybe.Maybe[B]].

    Person.persons foreach { p =>
      assert(Just(Just(p)).flatten == Just(p))
    }

    assert(Just(MaybeNot).flatten == MaybeNot)

    assert(MaybeNot.flatten == MaybeNot)
  }

  behavior of "Maybe.flatMap"
  it should "flatten a Maybe[Maybe[_]] according to monadic laws" in {

    def altFlatMap(m: Maybe[Person], f: Person => Maybe[Person]): Maybe[Person] = m.map(f).flatten

    val maybes = MaybeNot +: (Person.persons map { Just(_) })
    maybes foreach { m =>
      assert(altFlatMap(m, _.mother) == (m flatMap { _.mother }))
    }    
  }
}
