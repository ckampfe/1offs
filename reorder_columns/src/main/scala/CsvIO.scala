package com.ckampfe.reorder_columns

import au.com.bytecode.opencsv._
import java.io.FileReader
import java.io.FileWriter

case class CsvIO(
  reader: CSVReader,
  writer: CSVWriter,
  originalHeaders: List[String]
) {
  def writeToOutputFile(newHeaders: List[String]) = {
    def readWriteLines(line: Option[Array[String]]): Unit = line match {
      case None      => reader.close()
      case Some(row) =>
        val mappedRow = originalHeaders.zip(row).toMap
        val reorderedRow = reorderMappedRow(mappedRow, newHeaders)
        writer.writeNext(reorderedRow.toArray)
        readWriteLines(Option(reader.readNext()))
    }

    println("starting conversion...")
    readWriteLines(Option(reader.readNext()))
  }

  // take a map, and order the vals according to
  // keys provided by a list. Keys must be complete.
  def reorderMappedRow[A](m: Map[A, A], keys: List[A]) = {
    def loop(l: List[A], result: List[A]): List[A] =
      if (l.isEmpty) result.reverse
      else loop(l.tail, m(l.head) :: result)

    loop(keys, List())
  }
}

object CsvIO {
  def apply(inName: String, outName: String, separatorChar: Char) = {
    // make new input and output objects and
    // initialize a new instance with them
    val reader  = new CSVReader(
      new FileReader(inName),
      ',',
      '"',
      '|'
    )

    val writer = new CSVWriter(
      new FileWriter(outName),
      separatorChar.charValue(),
      CSVWriter.NO_QUOTE_CHARACTER
    )

    val originalHeaders = reader.readNext().toList

    new CsvIO(reader, writer, originalHeaders)
  }
}
