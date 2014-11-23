package com.ckampfe.reorder_columns

import java.io.FileReader

import akka.actor.{ Actor, ActorRef, Props }
import com.opencsv.CSVReader

class Reader(csvReader: CSVReader, arranger: ActorRef) extends Actor {
  def receive = {
    case ("start", newHeaders: List[String]) =>
      println("started read")
      readLoop(newHeaders)
  }

  def readLoop(newHeaders: List[String]): Unit = {
    def loop(line: Option[Array[String]]): Unit = line match {
      case None    =>
        println("done reading")
        csvReader.close()
        arranger ! ('ok, 'done)
        ()
      case Some(l) =>    // more, keep doing work
        arranger ! l
        loop(Option(csvReader.readNext()))
    }

    loop(Option(csvReader.readNext()))
  }
}

object Reader {
  def props(csvReader: CSVReader, arranger: ActorRef): Props = {
    Props(new Reader(csvReader, arranger))
  }
}
