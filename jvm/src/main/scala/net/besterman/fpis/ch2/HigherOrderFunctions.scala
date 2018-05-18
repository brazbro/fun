package net.besterman.fpis.ch2

object HigherOrderFunctions {

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    // Curry
    val addInt = curry((a: Int, b: Int) => a + b)
    val mpyInt = curry((a: Int, b: Int) => a * b)
    val add3 = addInt(3)
    val mpy12 = mpyInt(12)
    println("8 + 3 = " + add3(8))
    println("4 * 12 = " + mpy12(4))

    // Uncurry
    val sumInts = uncurry((a: Int) => (b: Int) => a + b)
    val sum = sumInts(5, 10)
    println("5 + 10 = " + sum)

    // Compose a function that takes a list of words and returns a concatenation where every word is capitalized
    val capitalize = (a: String) => a.toLowerCase().capitalize
    val concatenate = (a: Array[String]) => a.mkString(" ")
    val makeSentence = compose(capitalize, concatenate)
    println(makeSentence(Array("the", "sMaLL", "doG", "barked", "loudly")))
  }
}
