package net.besterman.problems;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class ProductOfIntsJava {

    public static int[] myAlgorithm1(int[] source) {
        assert source != null && source.length >= 2 : "Source must have at least 2 elements";
        int len = source.length;
        int[] target = new int[len];
        AtomicInteger numInvocations = new AtomicInteger(0);
        AtomicInteger numMultiplications = new AtomicInteger(0);
        Map<String, Integer> cache = new HashMap<>();
        for (int i = 0; i < len; i++) {
            target[i] = product(source, 0, i, cache, numInvocations, numMultiplications) *
                    product(source, i + 1, len, cache, numInvocations, numMultiplications);
            numMultiplications.incrementAndGet();
        }
        System.out.println("invocations=" + numInvocations + ", multiplications=" + numMultiplications);
        return target;
    }

    private static int product(int[] source, int from, int to, Map<String, Integer> cache,
                               AtomicInteger numInvocations, AtomicInteger numMultiplications) {
        numInvocations.incrementAndGet();
        if (from == to || from == source.length) {
            return 1;
        }

        if (from + 1 == to) {
            return source[from];
        }

        String key = "" + from + "-" + to;
        if (cache != null && cache.containsKey(key)) {
            return cache.get(key);
        }

        int product = source[from] * product(source, from + 1, to, cache, numInvocations, numMultiplications);
        numMultiplications.incrementAndGet();
        if (cache != null) {
            cache.put(key, product);
        }
        return product;
    }

    /**
     * This approach uses 3n space and 2n time
     */
    public static int[] myAlgorithm2(int[] source) {
        assert source != null && source.length >= 2 : "Source must have at least 2 elements";
        /*
           With 10 elements (0-9), need 2x9 = 18 products
                0, 0-1, 0-2, 0-3, 0-4, 0-5, 0-6, 0-7, 0-8, 9, 1-9, 2-9, 3-9, 4-9, 5-9, 6-9, 7-9, 8-9(, 9)
         */
        int len = source.length;
        int[] target = new int[len];
        int numProducts = len * 2 - 1;
        int[] products = new int[numProducts];
        products[0] = source[0];
        products[len - 1] = products[numProducts - 1] = source[len - 1];
        for (int i = 1; i < len; i++) {
            products[i] = products[i - 1] * source[i];
            products[numProducts - (i + 1)] = products[numProducts - i] * source[len - (i + 1)];
        }

        //target[0] = products[10]; // 1 - 9
        //target[1] = products[0] * products[11]; // 2-9
        //target[2] = products[1] * products[12]; // 3-9

        for (int i = 0; i < len; i++) {
            int leftIdx = i - 1;
            int rightIdx = i + len;
            target[i] = (leftIdx < 0 ? 1 : products[leftIdx]) * (rightIdx >= numProducts ? 1 : products[rightIdx]);
        }

        return target;
    }

    /**
     * Uses n space and 2n time
     */
    public static int[] optimalAlgorithm(int[] source) {
        assert source != null && source.length >= 2 : "Source must have at least 2 elements";
        int len = source.length;
        int[] target = new int[len];

        // Start by calculating products to left of source[index]
        target[0] = 1;
        for (int i = 1; i < len; i++) {
            target[i] = source[i - 1] * target[i - 1];
        }

        // Calculate products to right of source[index}, multiply by products-to-left and store in target. This is the final result.
        int productToRight = 1;
        for (int i = len - 2; i >= 0; i--) { // Skip last index since there are no numbers to the right
            target[i + 1] *= productToRight;
            productToRight *= source[i + 1];
        }
        target[0] = productToRight;
        return target;
    }
}
