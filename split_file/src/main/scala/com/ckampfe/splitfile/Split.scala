package com.ckampfe.splitfile

import scala.io.Source
import scala.sys.process
import java.io._

/**
 * ***WARNING***: does not preserve line order.
 * Splits are determined by linenumber % numOfSplits
 */

object Split {
  /**
   * args(0): inFileName
   * args(1): # of splits
   */
  def main(args: Array[String]): Unit = {
    if (args.length < 2) error("<input file> <number of splits>")

    val inFileName  = args(0)
    val numOfSplits = args(1).toLong

    val fileIterator        = Source.fromFile(inFileName).getLines
    val linesWithNumbers    = fileIterator.zipWithIndex.toIndexedSeq
    val linesMappedToSplits = mapLinesToSplits(linesWithNumbers, numOfSplits)

    writeSplits(linesMappedToSplits, inFileName)
  }

  def mapLinesToSplits(
    linesWithNumbers: Seq[(String, Int)],
    numOfSplits: Long): Map[Long, Seq[String]] = {
    linesWithNumbers.
    groupBy { case (line, number) => number % numOfSplits }.
    map     { case (splitNo, lineAndLineNumber) =>
      (splitNo, lineAndLineNumber.map { case (line, _) => line })
    }
  }

  def writeSplits(
    linesMappedToSplits: Map[Long, Seq[String]],
    inFileName: String): Unit = {
    val writeMap = linesMappedToSplits.map { case (splitNo, lines) =>
      (new PrintWriter(new File(inFileName + "-" + splitNo)), lines)
    }

    writeMap.par.foreach { case (writer, lines) =>
      lines.foreach { line => writer.write(line + "\n") }
      writer.close()
    }
  }
}
