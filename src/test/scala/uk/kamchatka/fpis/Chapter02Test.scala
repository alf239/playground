package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.Chapter02.{fibonacci, isSorted}

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

  describe("isSorted") {
    it("is true for 1,2,3,4,5,6,7,8,9") {
      assert(isSorted[Int]((1 to 10).toArray, _ <= _))
    }
    it("is falsefor 1,3,2") {
      assert(!isSorted[Int](Array(1, 3, 2), _ <= _))
    }
    it("is true for a single element") {
      assert(isSorted[Int](Array(999), _ <= _))
    }
    it("is true for an empty array") {
      assert(isSorted[Int](Array(999), _ <= _))
    }
    it("is compatible with the standard sorting") {
      forAll { (as: Array[Int]) =>
        val sorted = as.sorted
        assert(isSorted[Int](sorted, _ <= _))
        assert(isSorted[Int](as, _ <= _) === (as sameElements sorted))
      }
    }
  }
}
