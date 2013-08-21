package monads.lisst

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BunnyInvasionSpec extends FlatSpec with ShouldMatchers {
  
  behavior of "Lisst.flatMap"
  it should "should produce generations as in the Haskell Bunny Invasion example" in {

    def generation(s: String) = Lisst(s, s, s)

    { Lisst("bunny") flatMap generation 
    } should equal {
      Lisst("bunny", "bunny", "bunny")
    }

    { Lisst("bunny") flatMap generation flatMap generation
    } should equal {
      Lisst("bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny")
    }

    // TODO: remove below code when tic tac toe example is completed

    import LisstMonad.KleisliComposition

    { Lisst("bunny") flatMap (generation _ kleisliCompose generation _)
    } should equal {
      Lisst("bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny")
    }

    def singleton(s: String) = Lisst(s)

    { (generation _ kleisliCompose generation _ kleisliCompose singleton _)("bunny")
    } should equal {
      Lisst("bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny")
    }

  }

}
