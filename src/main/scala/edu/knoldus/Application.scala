package edu.knoldus

import org.apache.log4j.Logger

object Application extends App {

  val log = Logger.getLogger(this.getClass)
  val listOperations = new ListOperations
  val listForLength = (1 to 10).toList
  val listLength = listOperations.findLength(listForLength)
  log.info(s"$listLength \n")

  //scalastyle:off
  val firstSequence = List(1, 2, 7, 6, 1, 2, 3)
  val secondSequence = List(1, 2, 3)
  //scalastyle:on

  val hasSequence = listOperations.hasSubsequence(firstSequence, secondSequence)
  if (hasSequence) {
    log.info("Sub sequence exists \n")
  }
  else {
    log.info("Sub sequence don't exists \n")
  }

  val concatenatedList = listOperations.concatenateLists(secondSequence, firstSequence)
  log.info(concatenatedList)

  val (firstSplittedList, secondSplittedList) = listOperations.splitList(firstSequence, (x: Int) => x%2 == 0)
  log.info(firstSplittedList)
  log.info(secondSplittedList)
}
