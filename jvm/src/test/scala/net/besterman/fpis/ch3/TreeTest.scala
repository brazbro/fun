package net.besterman.fpis.ch3

import net.besterman.fpis.ch3.Tree._
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreeTest extends FunSpec {

  val tree1 =
    Branch(
      Branch(
        Leaf(3),
        Branch(
          Leaf(500),
          Leaf(-21)
        )
      ),
      Leaf(7))


  describe("Tree.size") {
    it("should return 1 for a single leaf") {
      assert(size(Leaf(3)) === 1)
      assert(sizeViaFold(Leaf(3)) === 1)
    }

    it("should return 7 for tree1") {
      assert(size(tree1) === 7)
      assert(sizeViaFold(tree1) === 7)
    }
  }

  describe("Tree.maximum") {
    it("should return 500 for tree1") {
      assert(maximum(tree1) === 500)
      assert(maximumViaFold(tree1) === 500)
    }
  }

  describe("Tree.depth") {
    it("should return 0 for single leaf") {
      assert(depth(Leaf(3)) === 0)
      assert(depthViaFold(Leaf(3)) === 0)
    }

    it("should return 4 for tree1") {
      assert(depth(tree1) === 3)
      assert(depthViaFold(tree1) === 3)
    }
  }

  describe("Tree.map") {
    it("should work for a simple Leaf") {
      assert(map(Leaf(-12))(_ * -2) === Leaf(24))
      assert(mapViaFold(Leaf(-12))(_ * -2) === Leaf(24))
    }

    it("should work for tree1") {
      val f = (i: Int) => i * 2 + 1
      val expected =
        Branch(
          Branch(
            Leaf(7),
            Branch(
              Leaf(1001),
              Leaf(-41)
            )
          ),
          Leaf(15))
      assert(map(tree1)(f) === expected)
      assert(mapViaFold(tree1)(f) === expected)
    }
  }
}
