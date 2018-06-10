package net.besterman.fpis.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * 3.25 function "size" that counts the number of nodes in a tree (leaves and branches)
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
    * 3.26 "maximum" function
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
    * 3.27 "depth" function
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  /**
    * 3.28 "map" function
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * 3.29 "fold" function
    */
  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => (1+l) max (1+r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}
