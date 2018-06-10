package net.besterman.fpis.ch2

object Sorted {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def checkOrder(idx: Int): Boolean = idx match {
      case x if x == as.length - 1 => true
      case _ => ordered(as(idx), as(idx + 1)) && checkOrder(idx + 1)
    }
    checkOrder(0)
  }

  val arr: Array[Int] = Array(1, 6, 45, 230)
  def main(args: Array[String]): Unit = arr.length match {
    case 0 => println("Empty")
    case _ => println("[" + arr.mkString(",") + "] is sorted? " + isSorted(arr, (i1: Int, i2: Int) => i2 >= i1))
  }
}
