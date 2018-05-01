package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.RNG.{nonNegative, toDouble}

class RngTest extends FunSpec with PropertyChecks {
  describe("nonNegative") {
    it("returns a nonnegative number") {
      forAll { n: Int => assert(nonNegative(n) >= 0) }
    }
    it("handles Int.MinValue") {
      assert(nonNegative(Int.MinValue) >= 0)
      assert(nonNegative(Int.MinValue) == Int.MaxValue)
    }
    it("keeps zero") {
      assert(nonNegative(0) == 0)
      assert(nonNegative(-1) == 0)
    }
  }
  describe("toDouble") {
    it("returns a nonnegative number") {
      forAll { n: Int => assert(toDouble(n) >= 0.0) }
    }
    it("returns a number less than 1.0") {
      forAll { n: Int => assert(toDouble(n) < 1.0) }
    }
  }
}
