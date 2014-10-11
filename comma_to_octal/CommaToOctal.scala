import au.com.bytecode.opencsv._
import java.io.FileReader
import java.io.FileWriter

object CommaToOctal {
  def main(args: Array[String]) = {
    val in     = args(0)
    val out    = args(1)
    val reader = new CSVReader(new FileReader(in))
    val writer = new CSVWriter(new FileWriter(out))
    writeToOutputFile(reader, writer)
  }

  def writeToOutputFile(inFile: CSVReader, writer: CSVWriter) = {
    println("starting conversion...")
    readWriteLines(inFile)

    def readWriteLines(inFile: CSVReader): Unit = Option(inFile.readNext()) match {
      case None    => inFile.close()
      case Some(l) =>
        writer.writeNext(l.mkString("\001,\001"))
        readWriteLines(inFile)
    }
  }
}
