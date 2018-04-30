package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.collections.Option.{sequence, traverse}

class OptionTest extends FunSpec with PropertyChecks {
  describe("None") {
    val none = collections.Option.none[Long]
    it("maps to None") {
      forAll { f: (Long => Double) =>
        assert(none.map(f) === collections.None)
      }
    }
    it("flatMaps to None") {
      forAll { f: (Long => Double) =>
        assert(none.flatMap(x => collections.Some(f(x))) === collections.None)
      }
    }
    it("defaults to default value") {
      forAll { d: Long =>
        assert(none.getOrElse(d) === d)
      }
    }
    it("defaults to the parameter") {
      forAll { d: Long =>
        assert(none.orElse(collections.Some(d)) === collections.Some(d))
        assert(none.orElse(collections.None) === collections.None)
      }
    }
    it("filters to None") {
      forAll { p: (Long => Boolean) =>
        assert(none.filter(p) === collections.None)
      }
    }
  }
  describe("Some") {
    it("maps to Some") {
      forAll { (x: Long, f: Long => Double) =>
        assert(collections.Some(x).map(f) === collections.Some(f(x)))
      }
    }
    it("flatMaps to Some") {
      forAll { (x: Long, f: Long => Double) =>
        assert(collections.Some(x).flatMap(x => collections.Some(f(x))) === collections.Some(f(x)))
      }
    }
    it("defaults to itself in getOrElse") {
      forAll { (x: Long, d: Long) =>
        assert(collections.Some(x).getOrElse(d) === x)
      }
    }
    it("defaults to itself in orElse") {
      forAll { (x: Long, d: Long) =>
        assert(collections.Some(x).orElse(collections.Some(d)) === collections.Some(x))
        assert(collections.Some(x).orElse(collections.None) === collections.Some(x))
      }
    }
    it("filters to Some if value satisfies the filter") {
      forAll { (x: Long, p: Long => Boolean) =>
        whenever(p(x)) {
          assert(collections.Some(x).filter(p) === collections.Some(x))
        }
      }
    }
    it("filters to None if value doesn't satisfy the filter") {
      forAll { (x: Long, p: Long => Boolean) =>
        whenever(!p(x)) {
          assert(collections.Some(x).filter(p) === collections.None)
        }
      }
    }
  }
  describe("sequence") {
    it("works for a simple example") {
      assert(sequence(collections.List(collections.Some(1), collections.Some(2), collections.Some(3))) === collections.Some(collections.List(1, 2, 3)))
    }
    it("works for a simple example with None") {
      assert(sequence(collections.List(collections.Some(1), collections.None, collections.Some(3))) === collections.None)
    }
  }
  describe("traverse") {
    it("works for a simple example") {
      assert(traverse(collections.List(1, 2, 3))(x => collections.Some(x * x)) === collections.Some(collections.List(1, 4, 9)))
    }
    it("works for a simple example with None") {
      assert(traverse(collections.List(1, 2, 3))(x => if (x % 2 == 0) collections.None else collections.Some(x * x)) === collections.None)
    }
  }
}
