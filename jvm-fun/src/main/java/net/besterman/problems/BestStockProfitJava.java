package net.besterman.problems;

/**
 * Problem: given a list of minute-by-minute stock prices where the indices are the time (in minutes) past trade opening time, and
 * the values are the price (in US dollars) of one share of Apple stock at that time, write an efficient function that
 * returns the best profit one could have made from one purchase and one sale of one share.
 * See https://www.interviewcake.com/question/python/stock-price
 */
public class BestStockProfitJava {

    /**
     * @param stockPrices a minute-by-minute array of stock prices, where the indices are the time (in minutes) past trade opening time
     * @return the best profit one could have made from one purchase and one sale of one share
     */
    public static int calculateMaxProfit(int[] stockPrices) {
        assert stockPrices != null && stockPrices.length > 1 : "At least 2 stock prices are required";
        Integer min = null;
        Integer max = null;
        Integer bestProfit = null;
        for (int price : stockPrices) {
            if (min == null) {
                min = price;
            } else if (price < min) {
                if (max == null) {
                    if (bestProfit == null) {
                        bestProfit = price - min;
                    } else {
                        bestProfit = Math.max(bestProfit, price - min);
                    }
                }
                min = price;
                max = null;
            } else if (max == null || price >= max) {
                max = price;
                if (bestProfit == null || max - min > bestProfit) {
                    bestProfit = max - min;
                }
            }
        }
        assert bestProfit != null;
        return  bestProfit;
    }

    /**
     * Optimal algorithm relies on defining the potential profit at any time as (current price - minimum price seen so far).
     * This must account for the potential of a loss as the maximum "profit".
     */
    public static int optimalAlgorithm(int[] stockPrices) {
        assert stockPrices != null && stockPrices.length > 1 : "At least 2 stock prices are required";
        int numPrices = stockPrices.length;
        int minPrice = stockPrices[0];
        int maxProfit = stockPrices[1] - minPrice;
        for (int t = 1; t < numPrices; t++) {
            int currentPrice = stockPrices[t];
            int potentialProfit = currentPrice - minPrice;
            maxProfit = Math.max(maxProfit, potentialProfit);
            minPrice = Math.min(minPrice, currentPrice);
        }
        return maxProfit;
    }
}
