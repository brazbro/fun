package net.besterman.problems

/**
  * https://www.interviewcake.com/question/python/merging-ranges
  */
object MergeTuples {

  def findOverlap(t1: (Int, Int), t2: (Int, Int)): Option[(Int, Int)] = {
    if (t1._1 <= t2._1) {
      if (t1._2 >= t2._2) Some(t1)
      else if (t1._2 >= t2._1) Some((t1._1, t2._2))
      else None
    }
    else if (t1._1 <= t2._2) {
      if (t1._2 >= t2._2) Some(t2._1, t1._2)
      else Some(t2._1, t2._2)
    }
    else None
  }

  def doMerge(arr: Array[(Int, Int)]): Array[(Int, Int)] = {
    val target = new collection.mutable.ArrayBuffer[(Int, Int)]
    var i = 0
    while (i < arr.length) {
      var tup = arr(i)
      var j = 0
      while (j < target.length) {
        val tj = target(j)
        val overlap = findOverlap(tj, tup)
        if (overlap.isDefined) {
          tup = overlap.get
          target.remove(j)
        } else j += 1
      }
      target.append(tup)
      i += 1
    }
    target.sorted.toArray
  }

  /**
    * Better approach is to sort the input list first, then take a single pass through instead of nested loops
    */

  private def tryMerge(prevMeeting: (Int, Int), nextMeeting: (Int, Int)): Option[(Int, Int)] = {
    if (nextMeeting._1 <= prevMeeting._2) Some((prevMeeting._1, prevMeeting._2.max(nextMeeting._2)))
    else None
  }

  def optimalMerge(meetings: Array[(Int, Int)]): collection.mutable.ArrayBuffer[(Int, Int)] = {
    assert(meetings != null)
    val mergedMeetings = new collection.mutable.ArrayBuffer[(Int, Int)]
    if (meetings.length < 2) mergedMeetings.appendAll(meetings)
    else {
      val sortedMeetings = meetings.sorted
      mergedMeetings.append(sortedMeetings(0))
      var i = 1
      while (i < sortedMeetings.length) {
        val nextMeeting = sortedMeetings(i)
        val merged = tryMerge(mergedMeetings.last, nextMeeting)
        if (merged.isDefined) mergedMeetings.update(mergedMeetings.length - 1, merged.get)
        else mergedMeetings += nextMeeting
        i += 1
      }
    }
    mergedMeetings
  }
}
