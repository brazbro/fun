package net.besterman;

/**
 * A Java app that references Java, Scala and Groovy classes
 */
public class PolyglotJavaApp {

    public static void main(String ... args) {
        System.out.println(HelloJava.getJavaHello());
        System.out.println(HelloGroovy.getGroovyHello());
        //System.out.println(HelloScala.getScalaHello());
    }
}
