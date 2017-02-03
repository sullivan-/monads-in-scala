package monads.lisst

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class LisstFlatMapSpec extends FlatSpec with Matchers {

  import testdata._

  behavior of "Lisst.flatMap"
  it should "apply f to each element in turn, and concatenate the results" in {

    emptyIntLisst.flatMap(toZeroStrings) should equal { emptyStringLisst }

    emptyIntLisst.flatMap(toOneString) should equal { emptyStringLisst }

    emptyIntLisst.flatMap(toThreeStrings) should equal { emptyStringLisst }

    sizeThreeIntLisst.flatMap(toZeroStrings) should equal { emptyStringLisst }

    sizeThreeIntLisst.flatMap(toOneString) should equal {
      Lisst("1", "2", "3")
    }

    sizeThreeIntLisst.flatMap(toThreeStrings) should equal {
      Lisst("1", "1", "1", "2", "2", "2", "3", "3", "3")
    }
  }

}
