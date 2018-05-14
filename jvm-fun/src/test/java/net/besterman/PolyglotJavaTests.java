package net.besterman;

import org.junit.Test;
import static org.junit.Assert.*;

public class PolyglotJavaTests {

    @Test
    public void testJavaUtils() {
        assertEquals("Hello from Java", HelloJava.getJavaHello());
    }

    @Test
    public void testGroovyUtils() {
        assertEquals("Hello from Groovy", HelloGroovy.getGroovyHello());
    }

    @Test
    public void testScalaUtils() {
        assertEquals("Hello from Scala", HelloScala.getScalaHello());
    }

}
