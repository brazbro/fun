package net.besterman.problems

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import MergeTuples._

@RunWith(classOf[JUnitRunner])
class MergeTuplesTest extends FunSpec {

  describe("MergeTuples") {
    it("should leave single tuple unchanged") {
      assert(optimalMerge(Array((3, 10))) === Array((3, 10)))
    }

    it("should sort 2 tuples") {
      assert(optimalMerge(Array((7, 8), (2, 4))) === Array((2, 4), (7, 8)))
    }

    it("should merge 2 meetings that abut each other") {
      assert(optimalMerge(Array((2, 4), (4, 7))) === Array((2, 7)))
      assert(optimalMerge(Array((4, 7), (2, 4))) === Array((2, 7)))
    }

    it("should not merge meetings separated by a gap") {
      assert(optimalMerge(Array((3, 6), (0, 1), (10, 12))) === Array((0, 1), (3, 6), (10, 12)))
    }

    it("should merge overlapping meetings") {
      assert(optimalMerge(Array((7, 20), (15, 22), (0, 3), (5, 10))) === Array((0, 3), (5, 22)))
    }

    it("should merge meetings that fully consume other meetings") {
      assert(optimalMerge(Array((3, 7), (7, 10), (6, 8), (1, 20), (15, 18))) === Array((1, 20)))
    }

    it("should not leave out the last meeting") {
      assert(optimalMerge(Array((3, 10), (2, 8), (13, 15))) === Array((2, 10), (13, 15)))
    }
  }
}
