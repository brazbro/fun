package net.besterman.problems;

import java.util.LinkedList;

/**
 * Problem: given a list of minute-by-minute stock prices where the indices are the time (in minutes) past trade opening time, and
 * the values are the price (in US dollars) of one share of Apple stock at that time, write an efficient function that
 * returns the best profit one could have made from one purchase and one sale of one share.
 * See https://www.interviewcake.com/question/python/stock-price
 */
public class BestStockProfitJava {

    /**
     * The approach is to realize that as long as the stock price is dropping, you can ignore everything but the lowest price,
     * and as long as the price is increasing, you can ignore everything but the highest price. These are local minimums and
     * maximums which you can extract into a new list in a single pass.
     * Then you make a second pass to calculate the largest interval between consecutive points.
     * @param stockPrices a minute-by-minute array of stock prices, where the indices are the time (in minutes) pas trade opening time
     * @return the best profit one could have made from one purchase and one sale of one share
     */
    public static int calculateMaxProfit(int[] stockPrices) {
        assert stockPrices != null && stockPrices.length > 1 : "At least 2 stock prices are required";
        LinkedList<Integer> prunedList = new LinkedList<>();
        boolean rising = true;
        for (int price : stockPrices) {
            if (prunedList.isEmpty()) {
                prunedList.add(price);
            } else if (price != prunedList.getLast()) { // ignore any prices that are same as previous price
                if (prunedList.size() == 1) {
                    // Determine initial direction
                    rising = (price > prunedList.getLast());
                    prunedList.add(price); // now that we have 2 elements, we can start pruning if direction does not change
                } else if (rising == (price > prunedList.getLast())) {
                    // going in same direction, so eliminate previous price
                    prunedList.removeLast();
                    prunedList.add(price);
                } else {
                    // Change of direction
                    prunedList.add(price);
                    rising = !rising;
                }
            }
        }

        // Special case of a constant price: no trend == no profit
        if (prunedList.size() == 1) {
            return 0;
        }

        // Special case of single downward trend: find minimum loss between prices
        if (prunedList.size() == 2 && prunedList.getFirst() > prunedList.getLast()) {
            int low = prunedList.getLast();
            int minLoss = Integer.MIN_VALUE;
            Integer lastPrice = null;
            for (int price : stockPrices) {
                if (lastPrice != null) {
                    minLoss = Math.max(minLoss, price - lastPrice);
                }
                if (price == low) {
                    break;
                }
                lastPrice = price;
            }
            return minLoss;
        }

        int maxProfit = Integer.MIN_VALUE;
        int lastPrice = prunedList.removeFirst();
        for (int price : prunedList) {
            maxProfit = Math.max(maxProfit, price - lastPrice);
            lastPrice = price;
        }
        return maxProfit;
    }
}
