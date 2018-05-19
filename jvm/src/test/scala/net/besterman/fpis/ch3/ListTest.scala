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
}
