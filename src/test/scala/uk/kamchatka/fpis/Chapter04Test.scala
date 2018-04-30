package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.Option.{sequence, traverse}

class Chapter04Test extends FunSpec with PropertyChecks {
  describe("None") {
    val none = Option.none[Long]
    it("maps to None") {
      forAll { f: (Long => Double) =>
        assert(none.map(f) === None)
      }
    }
    it("flatMaps to None") {
      forAll { f: (Long => Double) =>
        assert(none.flatMap(x => Some(f(x))) === None)
      }
    }
    it("defaults to default value") {
      forAll { d: Long =>
        assert(none.getOrElse(d) === d)
      }
    }
    it("defaults to the parameter") {
      forAll { d: Long =>
        assert(none.orElse(Some(d)) === Some(d))
        assert(none.orElse(None) === None)
      }
    }
    it("filters to None") {
      forAll { p: (Long => Boolean) =>
        assert(none.filter(p) === None)
      }
    }
  }
  describe("Some") {
    it("maps to Some") {
      forAll { (x: Long, f: Long => Double) =>
        assert(Some(x).map(f) === Some(f(x)))
      }
    }
    it("flatMaps to Some") {
      forAll { (x: Long, f: Long => Double) =>
        assert(Some(x).flatMap(x => Some(f(x))) === Some(f(x)))
      }
    }
    it("defaults to itself in getOrElse") {
      forAll { (x: Long, d: Long) =>
        assert(Some(x).getOrElse(d) === x)
      }
    }
    it("defaults to itself in orElse") {
      forAll { (x: Long, d: Long) =>
        assert(Some(x).orElse(Some(d)) === Some(x))
        assert(Some(x).orElse(None) === Some(x))
      }
    }
    it("filters to Some if value satisfies the filter") {
      forAll { (x: Long, p: Long => Boolean) =>
        whenever(p(x)) {
          assert(Some(x).filter(p) === Some(x))
        }
      }
    }
    it("filters to None if value doesn't satisfy the filter") {
      forAll { (x: Long, p: Long => Boolean) =>
        whenever(!p(x)) {
          assert(Some(x).filter(p) === None)
        }
      }
    }
  }
  describe("sequence") {
    it("works for a simple example") {
      assert(sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
    }
    it("works for a simple example with None") {
      assert(sequence(List(Some(1), None, Some(3))) === None)
    }
  }
  describe("traverse") {
    it("works for a simple example") {
      assert(traverse(List(1, 2, 3))(x => Some(x * x)) === Some(List(1, 4, 9)))
    }
    it("works for a simple example with None") {
      assert(traverse(List(1, 2, 3))(x => if (x % 2 == 0) None else Some(x * x)) === None)
    }
  }
}
