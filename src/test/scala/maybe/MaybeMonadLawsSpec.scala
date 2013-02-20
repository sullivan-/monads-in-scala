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
}
