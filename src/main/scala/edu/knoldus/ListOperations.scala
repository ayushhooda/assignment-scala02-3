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

  def splitList[T](list: List[T], f: T => Boolean): (List[T], List[T]) = {
    def innerSplit[T](list: List[T], firstList: List[T], secondList: List[T], f: T => Boolean): (List[T], List[T]) = {
      list match {
        case head :: tail => if (f(head)) {
          innerSplit(tail, firstList :+ head, secondList, f)
        } else {
          innerSplit(tail, firstList, secondList :+ head, f)
        }
        case Nil => (firstList, secondList)
      }
    }

    innerSplit(list, List(), List(), f)
  }


}
