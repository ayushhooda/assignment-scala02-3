package edu.knoldus

class ListOperations {

  def findLength[T](list: List[T]): Int = {
    list.foldRight(0)((_, length) => length + 1)
  }

  def concatenateLists[T](firstList: List[T], secondList: List[T]): List[T] = {
    firstList match {
      case firstElement :: tail => concatenateLists(tail, secondList :+ firstElement)
      case Nil => secondList
    }
  }

  def hasSubsequence[T](mainList: List[T], subList: List[T]): Boolean = {
    def innerOperate[T](mainList: List[T], subList: List[T], tempList: List[T], count: Int, temp: Int): Boolean = {
      mainList match {
        case head :: tail => if (head == subList.head) {
          innerOperate(tail, subList.tail, tempList, count - 1, temp)
        } else {
          innerOperate(tail, tempList, tempList, temp, temp)
        }
        case Nil => if (count == 0) true else false
      }
    }

    innerOperate(mainList, subList, subList, subList.length, subList.length)
  }

}
