package net.besterman.fpis.ch3

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import List._

@RunWith(classOf[JUnitRunner])
class ListTest extends FunSpec {

  describe("List") {
    it("should be Nil if empty") {
      val emptyList = List()
      assert(emptyList === Nil)
    }
  }

  describe("List.drop") {

    it("should return Nil for empty list") {
      val emptyList = List()
      assert(drop(emptyList, 0) === Nil)
      assert(drop(emptyList, 1) === Nil)
      assert(drop(emptyList, 2) === Nil)
    }

    it("should return Nil if n >=length of list") {
      val listof1 = List("one")
      assert(drop(listof1, 1) === Nil)
      assert(drop(listof1, 2) === Nil)
      assert(drop(listof1, 3) === Nil)
      assert(drop(listof1, 100) === Nil)

      val listOf10 = List(2, 5, 7, 8, 1, 2, 34, 6, -12, 234)
      assert(drop(listOf10, 9) != Nil)
      assert(drop(listOf10, 10) === Nil)
      assert(drop(listOf10, 11) === Nil)
    }

    it("should return list with n elements dropped from head") {
      val myList = List("Dr.", "Brian", "J.", "Besterman")
      assert(drop(myList, 0) === List("Dr.", "Brian", "J.", "Besterman"))
      assert(drop(myList, 1) === List("Brian", "J.", "Besterman"))
      assert(drop(myList, 2) === List("J.", "Besterman"))
      assert(drop(myList, 3) === List("Besterman"))
      assert(drop(myList, 4) === Nil)
      assert(drop(myList, 5) === Nil)
    }
  }

  describe("List.dropWhile") {

    it("should return Nil for empty list") {
      val emptyList = List()
      assert(dropWhile(emptyList)(_ => true) === Nil)
      assert(dropWhile(emptyList)(_ => false) === Nil)
    }

    it("should return original list if predicate always false") {
      val list = List(1, 2, 3, 4, 5)
      assert(dropWhile(list)(_ => false) === list)
      assert(dropWhile(list)(a => a > 5) === list)
    }

    it("should return Nil if predicate always true") {
      val list = List(true, true, false, true, false, false)
      assert(dropWhile(list)(_ => true) === Nil)
    }

    it("should return tail of list if first element matches predicate and 2nd doesn't, even if other elements match the predicate") {
      val list = List(0, 1, 2, 3, 4, 5, 6, 7)
      assert(dropWhile(list)(a => a == 0) === tail(list))
      assert(dropWhile(list)(a => a < 1 || a >= 3) === tail(list))
    }
  }

  describe("List.init") {
    it("should return all elements but the last") {
      val list = List(1, 2, 3, 4, 5)
      assert(init(list) === List(1, 2, 3, 4))

      val list2 = List(1)
      assert(init(list2) === Nil)
    }
  }

  private def doFold[A, B](l: List[A], z: B)(f: (B, A) => B)(foldFn: (List[A], B) => ((B, A) => B) => B): B = {
    foldFn(l, z)(f)
  }

  private def doFoldLeftTests(foldFn: (List[Int], Int) => ((Int, Int) => Int) => Int) = {
    val list = List(1, 2, 3, 4)
    val sum = doFold(list, 0)(_ + _)(foldFn)
    assert(sum === 10)
    val product = doFold(list, 1)(_ * _)(foldFn)
    assert(product === 24)
    val length = doFold(list, 0)((acc, _) => acc + 1)(foldFn)
    assert(length === 4)
  }

  describe("List.foldLeft") {
    it("My foldLeft should work") {
      doFoldLeftTests(foldLeft)
    }

    it("Better foldLeft should work") {
      doFoldLeftTests(foldLeft2)
    }
  }

  describe("List: fold functions") {
    it("flSum should work") {
      assert(flSum(List(1, 2, 3, 4, 5)) === 15)
      assert(flSum(List()) === 0)
      assert(flSum(Nil) === 0)
    }

    it("flProduct should work") {
      assert(flProduct(List(1, 2, 3, 4, 5)) === 120)

      // Empty product is 1 (https://en.wikipedia.org/wiki/Empty_product)
      assert(flProduct(List()) === 1)
      assert(flProduct(Nil) === 1)
    }

    it("flLength should work") {
      assert(flLength(Nil) === 0)
      assert(flLength(List()) === 0)
      assert(flLength(List(8, 7, 6, 5, 4, 3, 2, 1)) === 8)
    }

    it("reverseList should work") {
      assert(reverseList(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
      assert(reverseList(List(1)) === List(1))
      assert(reverseList(Nil) === Nil)
    }

    it("append should work") {
      assert(append2(List(1, 2, 3, 4), List(5, 6, 7, 8)) === List(1, 2, 3, 4, 5, 6, 7, 8))
      //intercept[Exception](append(Nil, List(1, 2, 3, 4)))
      assert(append2(Nil, List(1, 2, 3, 4)) === List(1, 2, 3, 4))
      assert(append2(List(1, 2, 3, 4), Nil) === List(1, 2, 3, 4))
    }

    it("concat should work") {
      assert(concat(List(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 0))) === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
      assert(concat(List(Nil, List(5, 6, 7, 8), List(9, 0))) === List(5, 6, 7, 8, 9, 0))
      assert(concat(List(List(1, 2, 3, 4), List(), List(9, 0))) === List(1, 2, 3, 4, 9, 0))
      assert(concat(List(List(1, 2, 3, 4), List(5, 6, 7, 8), Nil)) === List(1, 2, 3, 4, 5, 6, 7, 8))
    }

    it("add1 should work") {
      assert(add1(List(-10, 3, -100, 22, 109)) === List(-9, 4, -99, 23, 110))
    }

    it("doubleToString should work") {
      assert(doubleToString(List(3.2, -9.01, 0.003)) === List("3.2", "-9.01", "0.003"))
    }
  }

  describe("List.map") {
    it("should generalize the add1 function") {
      val l = List(1, 2, 3, 4, 5)
      assert(map(l)(_ + 1) === add1(l))
    }

    it("should generalize the doubleToString function") {
      val l = List(3.2, -9.01, 0.003)
      assert(map(l)(_.toString) === doubleToString(l))
    }
  }

  /* Filter odd numbers from a list */
  def filterTest(filterFn: List[Int] => (Int => Boolean) => List[Int]): Unit = {
    assert(filterFn(List(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_%2==0) === List(-2, 0, 2, 4, 6, 8, 10))
  }

  describe("List.filter in its various forms") {
    it("should filter all odd numbers from a list") {
      filterTest(filter)
      filterTest(filter2)
      filterTest(filter3)
      filterTest(filter4)
    }
  }

  /* Double up elements in a list */
  def flatMapTest(flatMapFn: List[Int] => (Int => List[Int]) => List[Int]): Unit = {
    assert(flatMapFn(List(1, 2, 3))(i => List(i, i)) === List(1, 1, 2, 2, 3, 3))
  }

  describe("List.flatMap in its various forms") {
    it("should work") {
      flatMapTest(flatMap)
      flatMapTest(flatMap2)
    }
  }

  describe("List.addLists") {
    it("should add lists of equal length") {
      assert(addLists(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
    }

    it("should add lists of unequal length") {
      assert(addLists(List(1, 2, 3, 4), List(5, 6)) === List(6, 8, 3, 4))
      assert(addLists(List(0, 7), List(1, 2, 3, 4)) === List(1, 9, 3, 4))
    }
  }

  describe("List.zipWith") {
    it("should zip lists of equal length") {
      assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_*_) === List(4, 10, 18))
      assert(zipWith(List("1", "2", "3"), List(4, 5, 6))(_.toInt * _) === List(4, 10, 18))
    }

    it("only zips lists of unequal length by stopping after the shorter list") {
      assert(zipWith(List(1, 2, 3, 4), List(5, 6))(_*_) === List(5, 12))
      assert(zipWith(List(1, 2), List("3", "4", "5", "6"))(_*_.toInt) === List(3, 8))
    }
  }
}
