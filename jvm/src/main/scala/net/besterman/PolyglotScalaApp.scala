package net.besterman

/**
  * Scala app that references Scala, Java and Groovy classes
  */
object PolyglotScalaApp {
  
  def main(args : Array[String]) {
    println( HelloJava.getJavaHello() )
    println( HelloGroovy.getGroovyHello() )
    println ( HelloScala getScalaHello )
  }
}
