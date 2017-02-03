package monads

import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/** Contains a method that verifies that a monad obeys the three monad laws described here:
  * http://en.wikibooks.org/wiki/Haskell/Understanding_monads#Monad_Laws
  */
trait MonadLawsVerifier {
  self: FlatSpec with Matchers =>

  /** Runs FlatSpec tests against the provided test data and functions.
    *
    * @tparam A TODO
    * @tparam B TODO
    * @tparam C TODO
    * @param monadName the name of the monad under test
    * @param monad TODO
    * @param testDataDescription a brief textual description of the test set
    * @param testItems a list of test items of type A to iterate over
    * @param f a test function from A to Maybe[B].
    * @param g a test function from B to Maybe[C].
    */
  def monadShouldObeyMonadLaws[A, B, C](
    monadName: String,
    monad: Monad)(
    testDataDescription: String,
    testAs: Seq[A],
    testMs: Seq[monad.M[A]],
    f: Function1[A, monad.M[B]],
    g: Function1[B, monad.M[C]]): Unit = {

    behavior of monadName + " monad with respect to " + testDataDescription

    it should "obey left unit monadic law" in {
      testAs foreach { a =>
        { monad.bindingOperation(monad.unitFunction(a), f)
        } should equal {
          f(a)
        }
      }
    }

    it should "obey right unit monadic law" in {
      testMs foreach { m =>
        { monad.bindingOperation(m, { a: A => monad.unitFunction(a) })
        } should equal {
          m
        }
      }
    }

    it should "obey associativity monadic law" in {
      testMs foreach { m =>
        { monad.bindingOperation(
            monad.bindingOperation(m, f),
            g)
        } should equal {
          monad.bindingOperation(
            m,
            { a: A => monad.bindingOperation(f(a), g) })
        }
      }
    }
  }

}
