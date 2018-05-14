package net.besterman.problems

object BestStockProfitScala {

  def optimalAlgorithm(prices:Array[Int]) : Int = {
    require(prices.length >= 2, "At least 2 prices must be specified!")

    var minPrice = prices(0)
    var maxProfit = prices(1) - prices(0)
    for (t <- 1 until prices.length) {
      val currentPrice = prices(t)
      val potentialProfit = currentPrice - minPrice
      maxProfit = Math.max(maxProfit, potentialProfit)
      minPrice = Math.min(minPrice, currentPrice)
    }
    maxProfit
  }
}
