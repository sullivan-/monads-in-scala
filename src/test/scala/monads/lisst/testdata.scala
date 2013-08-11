package monads.lisst

object testdata {

  val emptyIntLisst = Lisst[Int]()
  val sizeThreeIntLisst = Lisst(1, 2, 3)
  val emptyStringLisst = Lisst[String]()
  val emptyDoubleLisst = Lisst[Double]()

  def toZeroStrings(i: Int): Lisst[String] = emptyStringLisst
  def toOneString(i: Int): Lisst[String] = Lisst(i.toString)
  def toThreeStrings(i: Int): Lisst[String] = {
    val s = i.toString
    Lisst(s, s, s)
  }

  def toZeroDoubles(s: String): Lisst[Double] = emptyDoubleLisst
  def toOneDouble(s: String): Lisst[Double] = Lisst(s.toDouble)
  def toThreeDoubles(s: String): Lisst[Double] = {
    val d = s.toDouble
    Lisst(d, d, d)
  }

}
