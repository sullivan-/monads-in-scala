package monads.lisst

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LisstFlatMapSpec extends FlatSpec with ShouldMatchers {

  def toEmpty(i: Int): Lisst[String] = Lisst()
  def toString(i: Int): Lisst[String] = Lisst(i.toString)
  def toThreeStrings(i: Int): Lisst[String] = {
    val s = i.toString
    Lisst(s, s, s)
  }

  val empty = Lisst[Int]()
  val sizeThree = Lisst(1, 2, 3)

  behavior of "Lisst.flatMap"
  it should "apply f to each element in turn, and concatenate the results" in {

    empty.flatMap(toEmpty) should equal (Lisst[String]())
    empty.flatMap(toString) should equal (Lisst[String]())
    empty.flatMap(toThreeStrings) should equal (Lisst[String]())

    sizeThree.flatMap(toEmpty) should equal (Lisst[String]())
    sizeThree.flatMap(toString) should equal {
      Lisst("1", "2", "3")
    }
    sizeThree.flatMap(toThreeStrings) should equal {
      Lisst("1", "1", "1", "2", "2", "2", "3", "3", "3")
    }
  }

}
