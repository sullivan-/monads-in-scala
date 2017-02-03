package monads.maybe

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class MaybePersonSpec extends FlatSpec with Matchers {

  def maternalGrandfather(p: Person): Maybe[Person] =
    p.mother flatMap { _.father }

  def maternalGrandfatherNoFlatMap(p: Person): Maybe[Person] =
    p.mother match {
      case Just(m) => m.father
      case MaybeNot => MaybeNot
    }

  behavior of "Maybe.flatMap"
  it should "propagate Maybes appropriately in maternalGrandfather example" in {
    Person.persons foreach { p =>
      maternalGrandfather(p) should equal (maternalGrandfatherNoFlatMap(p))
    }
  }

  def bothGrandfathersFlatMap(p: Person): Maybe[(Person, Person)] =
    p.mother flatMap { m =>
      m.father flatMap { fm =>
        p.father flatMap { f =>
          f.father flatMap { ff =>
            Just(fm, ff)
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

  def assertBothGrandfathers(
    bothGrandfathers1: Person => Maybe[(Person, Person)],
    bothGrandfathers2: Person => Maybe[(Person, Person)]) =
    Person.persons foreach { p =>
      bothGrandfathers1(p) should equal (bothGrandfathers2(p))
    }

  behavior of "Maybe.flatMap"
  it should "propagate Maybes appropriately in bothGrandfathersFlatMap example" in {
    assertBothGrandfathers(bothGrandfathersFlatMap, bothGrandfathersNoFlatMap)
    assertBothGrandfathers(bothGrandfathersFlatMap, bothGrandfathersNoFlatMapNoPairMatch)
  }

  def bothGrandfathersForLoop(p: Person): Maybe[(Person, Person)] =
    for(
      m <- p.mother;
      fm <- m.father;
      f <- p.father;
      ff <- f.father)
    yield (fm, ff)

  behavior of "Maybe.map"
  it should "propagate Maybes appropriately in bothGrandfathersForLoop example" in {
    assertBothGrandfathers(bothGrandfathersForLoop, bothGrandfathersFlatMap)
  }
}
