package net.besterman.fpis.ch4

//hide std library `Option` and `Either`
import com.sun.xml.internal.messaging.saaj.soap.SOAPVersionMismatchException

import scala.{Either => _, Option => _, Some => _, _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /**
    * Wrap "get" in another Some to create an Option; now B is the right type to return. Tricky!
    */
  def orElse_2[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(s) if f(s) => this
    case _ => None
  }

  def filter_2(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}