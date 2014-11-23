package com.ckampfe.reorder_columns

import akka.actor.{ Actor, Props }
import com.opencsv.CSVWriter

class Writer(csvWriter: CSVWriter) extends Actor {
  def receive = {
    case (line: Array[String]) =>
      csvWriter.writeNext(line)
    case ('ok, 'done) =>
        println("DONE")
        csvWriter.close()
        context.system.shutdown()
        System.exit(0)
  }
}

object Writer {
  def props(csvWriter: CSVWriter): Props = Props(new Writer(csvWriter))
}
