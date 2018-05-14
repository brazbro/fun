package net.besterman.problems;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class BestStockProfitJavaTest {

    interface ProfitCalculator {
        int calculateMaxProfit(int[] prices);
    }

    private final ProfitCalculator MY_CALCULATOR = BestStockProfitJava::calculateMaxProfit;
    private final ProfitCalculator OPTIMAL_CALCULATOR = BestStockProfitJava::optimalAlgorithm;
    private ProfitCalculator calculatorUnderTest = MY_CALCULATOR;

    @Before
    public void init() {
        calculatorUnderTest = OPTIMAL_CALCULATOR;
    }

    @Test
    public void testZeroProfit() {
        int[] noProfit = {10, 10, 10, 10, 10, 10};
        assertEquals(0, calculatorUnderTest.calculateMaxProfit(noProfit));
    }

    @Test
    public void testLoss() {
        // To have a loss, first price of day was the high
        int[] loss = {100, 90, 70, 68, 66, 63, 60};
        assertEquals(-2, calculatorUnderTest.calculateMaxProfit(loss));
    }

    @Test
    public void testLossFollowedByBounce() {
        int[] loss = {100, 90, 70, 68, 66, 63, 60, 61};
        assertEquals(1, calculatorUnderTest.calculateMaxProfit(loss));
    }

    @Test
    public void testRisingTrend() {
        int[] rising = {100, 105, 130, 180, 202};
        assertEquals(102, calculatorUnderTest.calculateMaxProfit(rising));
    }

    @Test
    public void testRisingThenFalling() {
        int[] risingThenFalling = {100, 105, 120, 200, 199, 197, 190};
        assertEquals(100, calculatorUnderTest.calculateMaxProfit(risingThenFalling));
    }

    @Test
    public void testTinyProfitThenCrash() {
        int[] trend = {1000, 1000, 1000, 1001, 1001, 1000, 999, 5, 4, 3, 2, 0};
        assertEquals(1, calculatorUnderTest.calculateMaxProfit(trend));
    }

    @Test
    public void testMultipleConsecutivePeaks() {
        int[] trend = {10, 100, 50, 200, 5};
        assertEquals(190, calculatorUnderTest.calculateMaxProfit(trend));
    }
}
