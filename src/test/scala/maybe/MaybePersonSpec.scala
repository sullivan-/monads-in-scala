package maybe

import org.scalatest.FlatSpec

class MaybePersonSpec extends FlatSpec {

  behavior of "Maybe.flatMap"
  it should "propagate MaybeNots appropriately" in {

    def maternalGrandfather(p: Person): Maybe[Person] =
      p.mother flatMap { _.father }

    def maternalGrandfatherNoFlatMap(p: Person): Maybe[Person] =
      p.mother match {
        case Just(m) => m.father match {
          case Just(fm) => Just(fm)
          case MaybeNot => MaybeNot
        }
        case MaybeNot => MaybeNot
      }

    Person.persons foreach { p =>
      assert(maternalGrandfather(p) == maternalGrandfatherNoFlatMap(p))
    }

    def bothGrandfathersFlatMap(p: Person): Maybe[(Person, Person)] =
      p.mother flatMap { m =>
        m.father flatMap { fm =>
          p.father flatMap { f =>
            f.father flatMap { ff =>
              Maybe(fm, ff)
            }
          }
        }
      }

    def bothGrandfathersNoFlatMap(p: Person): Maybe[(Person, Person)] =
      (p.mother, p.father) match {
        case (Just(m), Just(f)) =>
          (m.father, f.father) match {
            case (Just(fm), Just(ff)) => Just((fm, ff))
            case _ => MaybeNot
          }
        case _ => MaybeNot
      }

    def assertBothGrandfathers(
      bothGrandfathers1: Person => Maybe[(Person, Person)],
      bothGrandfathers2: Person => Maybe[(Person, Person)]) =
      Person.persons foreach { p =>
        assert(bothGrandfathers1(p) == bothGrandfathers2(p))
      }

    assertBothGrandfathers(bothGrandfathersFlatMap, bothGrandfathersNoFlatMap)
    
    def bothGrandfathersNoFlatMapNoPairMatch(p: Person): Maybe[(Person, Person)] =
      p.mother match {
        case Just(m) =>
          p.father match {
            case Just(f) =>
              m.father match {
                case Just(fm) =>
                  f.father match {
                    case Just(ff) => Just((fm, ff))
                    case _ => MaybeNot
                  }
                case MaybeNot => MaybeNot
              }
            case MaybeNot => MaybeNot
          }
        case MaybeNot => MaybeNot
      }

    assertBothGrandfathers(bothGrandfathersFlatMap, bothGrandfathersNoFlatMapNoPairMatch)

    def bothGrandfathersForLoop(p: Person): Maybe[(Person, Person)] =
      for(
        m <- p.mother;
        fm <- m.father;
        f <- p.father;
        ff <- f.father)
      yield (fm, ff)

    assertBothGrandfathers(bothGrandfathersForLoop, bothGrandfathersFlatMap)
  }
}
