package net.besterman.fpis.ch2

object Fibonacci {

  /**
    * Returns the nth fibonacci number starting from index 0
    * @param n the index, where the first fibonacci number is at index 0
    * @return the nth fibonacci number
    */
  def fib(n: Int): BigInt = {

    @annotation.tailrec
    def recurse(n: Int, penultimate: BigInt, last: BigInt) : BigInt = n match {
      case 0 => penultimate
      case _ => recurse(n - 1, last, last + penultimate)
    }

    recurse(n, 0, 1)
  }

  def nFibs(n: Int): List[BigInt] = {
    List.tabulate(n)(fib)
  }

  val N = 1

  def main(args: Array[String]): Unit = {
    printf("nFibs(%d) = %s", N, nFibs(N))
  }
}
