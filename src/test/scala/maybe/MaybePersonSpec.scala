package maybe

import org.scalatest.FlatSpec

class MaybePersonSpec extends FlatSpec {

  behavior of "Maybe.flatMap"
  it should "propagate MaybeNots appropriately" in {

    def maternalGrandfather(p: Person): Maybe[Person] =
      p.mother flatMap { _.father }

    def paternalGrandfather(p: Person): Maybe[Person] =
      p.father flatMap { _.father }

    Person.persons foreach { p =>
      assert(maternalGrandfather(p) == (
        p.mother match {
          case Just(m) => m.father match {
            case Just(fm) => Just(fm)
            case MaybeNot => MaybeNot
          }
          case MaybeNot => MaybeNot
        }))
    }

    def bothGrandfathersFlatMaps(p: Person): Maybe[(Person, Person)] =
      p.mother flatMap { m =>
        m.father flatMap { fm =>
          p.father flatMap { f =>
            f.father flatMap { ff =>
              Maybe(fm, ff)
            }
          }
        }
      }

    def assertBothGrandfathers(bothGrandfathers: Person => Maybe[(Person, Person)]) =
      Person.persons foreach { p =>
        assert(bothGrandfathers(p) == (
          maternalGrandfather(p) match {
            case Just(fm) => paternalGrandfather(p) match {
              case Just(ff) => Just(fm, ff)
              case MaybeNot => MaybeNot
            }
          case MaybeNot => MaybeNot
          }))
      }

    assertBothGrandfathers(bothGrandfathersFlatMaps)

    def bothGrandfathersForLoop(p: Person): Maybe[(Person, Person)] =
      for(
        m <- p.mother;
        fm <- m.father;
        f <- p.father;
        ff <- f.father)
      yield (fm, ff)

    assertBothGrandfathers(bothGrandfathersForLoop)
  }
}
