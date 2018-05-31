package net.besterman.problems

import scala.collection.mutable.ListBuffer
import scala.util.Sorting

/**
  * Given an array of integers, find the highest product you can get from three of the integers
  */
object HighestProductOf3 {

  /**
    * Approach: Order positive and negative ints separately from low to high
    * Then if there are 2 or more negative ints, compute the product of the smallest 2 values. Compare the product of this
    * and the highest positive int to the product of the top 3 positive ints -> the answer is the higher of the 2 products
    * @param ints an array of integers
    */
  def mySolution(ints: Array[Int]): Int = ints.length match {
    case n if 0 to 2 contains n => sys.error("Array must have 3 or more elements")
    case 3 => ints(0) * ints(1) * ints(2)
    case _ => {
      val posList: ListBuffer[Int] = new ListBuffer()
      val negList: ListBuffer[Int] = new ListBuffer()
      for (i <- ints) {
        if (i >= 0) posList += i
        else negList += i
      }
      val positives = posList.sorted
      val negatives = negList.sorted
      if (positives.isEmpty) {
        negatives.slice(negatives.size-3, negatives.size).product
      }
      else if (negatives.size <= 1) {
        // Return top 3 positives
        positives.slice(positives.size-3, positives.size).product
      } else {
        // Check smallest 2 negatives
        val option1 = negatives.take(2).product * positives.takeRight(1).product
        val option2 = positives.takeRight(3).product
        if (option1 > option2) option1 else option2
      }
    }
  }

  def nlogn(ints: Array[Int]): Int = ints.length match {
    case n if 0 to 2 contains n => sys.error("Array must have 3 or more elements")
    case 3 => ints(0) * ints(1) * ints(2)
    case _ => {
      // Sort array in place O(nlog(n))
      Sorting.quickSort(ints)
      val option1 = ints(0) * ints(1) * ints.last
      val option2 = ints.takeRight(3).product
      Math.max(option1, option2)
    }
  }

  def orderN(ints: Array[Int]): Int = ints.length match {
    case n if 0 to 2 contains n => sys.error("Array must have 3 or more elements")
    case 3 => ints(0) * ints(1) * ints(2)
    case _ => {
      var pos1 = None: Option[Int]
      var pos2 = None: Option[Int]
      var pos3 = None: Option[Int]
      var neg3 = None: Option[Int]
      var neg2 = None: Option[Int]
      var neg1 = None: Option[Int]
      for (i <- ints) {
        if (i >= 0) {
          if (pos1.isEmpty) pos1 = Some(i)
          else if (i > pos1.get) {
            pos3 = pos2
            pos2 = pos1
            pos1 = Some(i)
          } else if (pos2.isEmpty) pos2 = Some(i)
          else if (i > pos2.get) {
            pos3 = pos2
            pos2 = Some(i)
          } else if (pos3.isEmpty || i > pos3.get) pos3 = Some(i)
        } else {
          // i is negative
          if (neg1.isEmpty) neg1 = Some(i)
          else if (i < neg1.get) {
            neg3 = neg2
            neg2 = neg1
            neg1 = Some(i)
          } else if (neg2.isEmpty) neg2=Some(i)
          else if (i < neg2.get) {
            neg3 = neg2
            neg2 = Some(i)
          } else if (neg3.isEmpty || i < neg3.get) neg3 = Some(i)
        }
      }

      // Option 1 is 3 positives
      val option1 = if (pos1.isDefined && pos2.isDefined && pos3.isDefined) Some(pos1.get * pos2.get * pos3.get) else None

      // Option 2 is 1 positive and 2 negatives
      val option2 = if (pos1.isDefined && neg2.isDefined && neg1.isDefined) Some(pos1.get * neg2.get * neg1.get) else None

      // Option 3 is 3 negatives (i.e. no positives)
      val option3 = if (option1.isEmpty) Some(neg1.get * neg2.get * neg3.get) else None

      Math.max(option1.getOrElse(Int.MinValue), Math.max(option2.getOrElse(Int.MinValue), option3.getOrElse(Int.MinValue)))
    }
  }
}
