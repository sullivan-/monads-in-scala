package maybe

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SafeMathSpec extends FlatSpec with ShouldMatchers {

  import safe._

  behavior of "various implementations of safe.safeLogSqrt"
  they should "agree over a range of test input" in {
    shouldAgreeOverTestInputRange(safeLogSqrt1 _, safeLogSqrt2 _)
    shouldAgreeOverTestInputRange(safeLogSqrt1 _, safeLogSqrt3 _)
    shouldAgreeOverTestInputRange(safeLogSqrt1 _, safeLogSqrt4 _)
    shouldAgreeOverTestInputRange(safeLogSqrt1 _, safeLogSqrt5 _)
  }

  def shouldAgreeOverTestInputRange(
    safeLogSqrt1: (Double) => Maybe[Double],
    safeLogSqrt2: (Double) => Maybe[Double]) {
    doubles foreach { d =>
      (safeLogSqrt1(d)) should equal (safeLogSqrt2(d))
    }
  }
}
