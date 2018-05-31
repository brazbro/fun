package net.besterman.problems

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import HighestProductOf3._

@RunWith(classOf[JUnitRunner])
class HighestProductOf3Test extends FunSpec {

  def dotest(ints: Array[Int]): Int = {
    orderN(ints)
  }

  describe("Highest product of 3") {

    it("should throw an error if the input is null") {
      assertThrows[Exception] {
        dotest(null)
      }
    }

    it("should throw an error if the input has fewer than 3 elements") {
      val emptyArr: Array[Int] = Array()
      assertThrows[RuntimeException] {
        dotest(emptyArr)
      }

      val arrayOf1 = Array(10)
      assertThrows[RuntimeException] {
        dotest(arrayOf1)
      }

      val arrayOf2 = Array(1, 2)
      assertThrows[RuntimeException] {
        dotest(arrayOf2)
      }
    }

    it("should return the product of largest 3 elements if array contains only positive ints") {
      assert(dotest(Array(7, 5, 10)) === 350)
      assert(dotest(Array(2, 10, 4, 1)) === 80)
      assert(dotest(Array(6, 4, 2, 0)) === 48)
      assert(dotest(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) === 720)
      assert(dotest(Array(3, 2, 1, 9, 4, 10, 6, 5, 8, 7)) === 720)
    }

    it("should return the product of largest 3 elements if array contains one negative element") {
      assert(dotest(Array(7, 5, 10, -100)) === 350)
      assert(dotest(Array(-2, 2, 2, 3)) === 12)
    }

    it("should handle multiple negative numbers correctly") {
      assert(dotest(Array(-10, -10, 1, 3, 2)) === 300)
      assert(dotest(Array(7, 5, 10, -100, -1)) === 1000)
      assert(dotest(Array(-20, 4, 6, -32, 9, 12, -100, 50)) === 160000)
    }
  }
}
