package net.besterman.problems;

import org.junit.Test;
import static org.junit.Assert.*;

public class ProductOfIntsJavaTest {

    @Test
    public void testTwo() {
        int[] expected = {50, 10};
        int[] actual = getProductsOfAllIntsExceptAtIndex(new int[]{10, 50});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testThree() {
        int[] expected = {18, 10, 45};
        int[] actual = getProductsOfAllIntsExceptAtIndex(new int[]{5, 9, 2});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testFour() {
        int[] expected = {-6, -4, -12, 6};
        int[] actual = getProductsOfAllIntsExceptAtIndex(new int[]{2, 3, 1, -2});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testTen() {
        int[] expected = {3456, 1728, 1152, 864, 1152, 1728, 3456, 1728, 1152, 864};
        int[] actual = getProductsOfAllIntsExceptAtIndex(new int[]{1, 2, 3, 4, 3, 2, 1, 2, 3, 4});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testZeroes() {
        int[] expected = {0, 0, 0, 0, 0};
        int[] actual = getProductsOfAllIntsExceptAtIndex(new int[]{1, 2, 0, 4, 0});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void smallArrayInput() {
        final int[] expected = new int[] {6, 3, 2};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {1, 2, 3});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void longArrayInput() {
        final int[] expected = new int[] {120, 480, 240, 320, 960, 192};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {8, 2, 4, 3, 1, 5});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void inputHasOneZero() {
        final int[] expected = new int[] {0, 0, 36, 0};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {6, 2, 0, 3});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void inputHasTwoZeros() {
        final int[] expected = new int[]{0, 0, 0, 0, 0};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {4, 0, 9, 1, 0});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void inputHasOneNegativeNumber() {
        final int[] expected = new int[] {32, -12, -24};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {-3, 8, 4});
        assertArrayEquals(expected, actual);
    }

    @Test
    public void allNegativesInput() {
        final int[] expected = new int[] {-8, -56, -14, -28};
        final int[] actual = getProductsOfAllIntsExceptAtIndex(new int[] {-7, -1, -4, -2});
        assertArrayEquals(expected, actual);
    }

    @Test(expected = AssertionError.class)
    public void exceptionWithEmptyInput() {
        getProductsOfAllIntsExceptAtIndex(new int[] {});
    }

    @Test(expected = AssertionError.class)
    public void exceptionWithOneNumberInput() {
        getProductsOfAllIntsExceptAtIndex(new int[] {1});
    }

    private int[] getProductsOfAllIntsExceptAtIndex(int[] source) {
        return ProductOfIntsJava.optimalAlgorithm(source);
    }
}
