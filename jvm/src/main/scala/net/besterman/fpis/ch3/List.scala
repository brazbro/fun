package net.besterman.fpis.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * 3.2
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot return tail of empty list!")
    case Cons(_, xs) => xs
  }

  /**
    * 3.3
    */
  def setHead[A](x: A, l: List[A]): List[A] = l match {
    case Nil => List(x)
    case Cons(_, xs) => Cons(x, xs)
  }

  /**
    * 3.4 Drop the first n elements of a list
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  /**
    * Alternative implementation of "drop". I think mine is a bit terser.
    */
  def dropAlt[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => dropAlt(t, n - 1)
    }
  }

  /**
    * 3.5 Remove elements from the front of a list as long as they match a predicate
    */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    // case Nil => Nil // Not needed!
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  /**
    * 3.6 return a List consisting of all but the last element of the given list
    * NOTE: This implementation can cause a stack overflow
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def initTailRec[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("Empty list!")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println("x = " + x)

    println(tail(List(3, 5, 12, -2, 34)))
  }
}
