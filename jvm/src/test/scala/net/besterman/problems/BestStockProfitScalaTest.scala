package net.besterman.problems

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BestStockProfitScalaTest extends FunSpec {

  describe("Best stock profit") {

    it("should report 0 profit if all prices are the same") {
      val noProfit = Array(10, 10, 10, 10, 10, 10)
      assert(BestStockProfitScala.optimalAlgorithm(noProfit) === 0)
    }

    it("should report a loss if prices continually drop") {
      val loss = Array(100, 90, 70, 68, 66, 63, 60)
      assert(BestStockProfitScala.optimalAlgorithm(loss) === -2)
    }

    it("should show a profit if a loss is followed by a bounce") {
      val loss = Array(100, 90, 70, 68, 66, 63, 60, 61)
      assert(BestStockProfitScala.optimalAlgorithm(loss) === 1)
    }

    it("should show correct profit on a continuously rising trend") {
      val rising = Array(100, 105, 130, 180, 202)
      assert(BestStockProfitScala.optimalAlgorithm(rising) === 102)
    }

    it("should show a correct profit on a rising then falling trend") {
      val risingThenFalling = Array(100, 105, 120, 200, 199, 197, 190)
      assert(BestStockProfitScala.optimalAlgorithm(risingThenFalling) === 100)
    }

    it("should show a profit if initial profit is followed by a crash") {
      val trend = Array(1000, 1000, 1000, 1001, 1001, 1000, 999, 5, 4, 3, 2, 0)
      assert(BestStockProfitScala.optimalAlgorithm(trend) === 1)
    }

    it("should handle multiple consecutive peaks as if there was a single peak from lowest to highest price") {
      val trend = Array(10, 100, 50, 200, 5)
      assert(BestStockProfitScala.optimalAlgorithm(trend) === 190)
    }
  }
}
