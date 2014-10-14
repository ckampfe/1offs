import au.com.bytecode.opencsv._
import java.io.FileReader
import java.io.FileWriter

object CommaToOctal {
  def main(args: Array[String]) = {
    val inName         = args(0)
    val outName        = args(1)

    val separatorChar  = args.lift(2) match {
      case None    => '\u0001'
      case Some(c) => c.toCharArray.apply(0)
    }

    val csvInput  = new CSVReader(
      new FileReader(inName),
      ',',
      '"',
      '|'
    )

    val csvOutput = new CSVWriter(
      new FileWriter(outName),
      separatorChar.charValue(),
      CSVWriter.NO_QUOTE_CHARACTER
    )

    writeToOutputFile(csvInput, csvOutput, separatorChar)
  }

  def writeToOutputFile(
    csvInput: CSVReader,
    csvOutput: CSVWriter,
    separatorChar: Char
  ) = {
    println("starting conversion...")

    readWriteLines()

    def readWriteLines(): Unit = Option(csvInput.readNext()) match {
      case None    => csvInput.close()
      case Some(l) =>
        csvOutput.writeNext(l)
        readWriteLines()
    }
  }
}
