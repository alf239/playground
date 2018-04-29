package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.Chapter02.fibonacci

class Chapter02Test extends FunSpec with PropertyChecks {
  describe("Fibonacci") {
    it("should be 0 at 0") {
      assert(fibonacci(0) === 0)
    }
    it("should be 1 at 1") {
      assert(fibonacci(1) === 1)
    }
    it("should be 1 at 2") {
      assert(fibonacci(2) === 1)
    }
    it("should be 2 at 3") {
      assert(fibonacci(3) === 2)
    }
    it("should maintain the recursive relation") {
      forAll { (n: Int) =>
        whenever(n >= 2) {
          assert(fibonacci(n) === fibonacci(n - 1) + fibonacci(n - 2))
        }
      }
    }
  }
}
