package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class RngTest extends FunSpec with PropertyChecks {
  describe("nonNegative") {
    it("returns a nonnegative number") {
      forAll { n: Int => assert(RNG.nonNegative(n) >= 0) }
    }
    it("handles Int.MinValue") {
      assert(RNG.nonNegative(Int.MinValue) >= 0)
      assert(RNG.nonNegative(Int.MinValue) == Int.MaxValue)
    }
    it("keeps zero") {
      assert(RNG.nonNegative(0) == 0)
      assert(RNG.nonNegative(-1) == 0)
    }
  }
}
