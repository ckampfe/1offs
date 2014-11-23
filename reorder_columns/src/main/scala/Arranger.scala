package com.ckampfe.reorder_columns

import akka.actor.{ Actor, ActorRef, Props }

class Arranger(
  newHeaders:      List[String],
  originalHeaders: List[String],
  writer:          ActorRef
) extends Actor {

  def receive = {
    case (line: Array[String]) =>
      val mappedRow = originalHeaders.zip(line).toMap
      writer ! reorderMappedRow(mappedRow, newHeaders).toArray
    case ('ok, 'done) =>
      writer ! ('ok, 'done)
  }

  def reorderMappedRow[A](m: Map[A, A], keys: List[A]) = {
    def loop(l: List[A], result: List[A]): List[A] =
      if (l.isEmpty) result.reverse
      else loop(l.tail, m(l.head) :: result)

    loop(keys, List())
  }
}

object Arranger {
  def props(
    newHeaders:      List[String],
    originalHeaders: List[String],
    writer:          ActorRef
  ): Props = Props(new Arranger(newHeaders, originalHeaders, writer))
}
