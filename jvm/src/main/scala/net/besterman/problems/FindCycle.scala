package net.besterman.problems

sealed trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Node[A](value: A, var next: LinkedList[A]) extends LinkedList[A] {
  override def toString: String = s"Node($value)"
}

object LinkedList {

  def apply[A](as: A): LinkedList[A] = Node(as, Nil)

  def node[A](n: LinkedList[A]): Node[A] = n.asInstanceOf[Node[A]]

  def hasCycle[A](l: LinkedList[A]): Boolean = {
    if (l == Nil) false
    else {
      var front = node(l)
      var rear = node(l)
      var done = false
      var foundCycle = false

      def checkForDone(): Unit = {
        if (front.next == Nil) done = true
        else if (front.next == rear) {
          foundCycle = true
          done = true
        }
      }

      while (!done) {
        checkForDone()
        if (!done) {
          front = node(front.next)
          checkForDone()
          if (!done) {
            front = node(front.next)
            rear = node(rear.next)
          }
        }
      }
      foundCycle
    }
  }

  def main(args : Array[String]): Unit = {
    val l1 = LinkedList(4)
    val l2 = LinkedList(10)
    val l3 = LinkedList(20)
    val l4 = LinkedList(22)
    val l5 = LinkedList(30)
    val l6 = LinkedList(50)
    node(l1).next = l2
    node(l2).next = l3
    node(l3).next = l4
    node(l4).next = l5
    node(l5).next = l6
    node(l6).next = l1
    println(l1 + " has cycle? " + hasCycle(l1))
  }
}

