import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CSVParserr {
  sealed trait ReturnType

  case class withHeader(header: List[String], list: List[Vector[String]]) extends ReturnType

  case class withoutHeader(list: List[Vector[String]]) extends ReturnType

  var unclosedField: String = ""
  val separator = ','
  val doubleQuotes = '"'
  val newLine = "\n"

  var isMultiline = false
  var unclosedFieldLine = new ListBuffer[String]


  def readCSV(path: String, isHeader: Boolean): ReturnType = {

    isMultiline = false
    val lines = scala.io.Source.fromFile(path).getLines().toVector
    if (isHeader) {
      val head = parse(lines.head).toList
      withHeader(head, readCSV(lines.tail))
    } else withoutHeader(readCSV(lines))
  }

  def readCSV(path: String, header: List[String]): List[Vector[String]] = {
    val lines = scala.io.Source.fromFile(path).getLines().toVector
    readCSV(lines)
  }

  def readCSV(lines: Vector[String]):List[Vector[String]] = {
    //val length = lines.length
    var result = new ListBuffer[Vector[String]]
    var i = 1
    lines.foreach(line => {

      val csvLineInArray = parse(line)
      if (isMultiline) {
        unclosedFieldLine.addAll(csvLineInArray)
      }
      else {

        if (unclosedFieldLine != null && unclosedFieldLine.nonEmpty) {
          var lineCSV = unclosedFieldLine ++ csvLineInArray
          result.append((lineCSV).toVector)
          unclosedFieldLine = new ListBuffer[String]
          if ((lineCSV).length != result(0).length) {
            throw new Exception(s"Each line should contain the same number of fields throughout the file. Error on line $i")
          }
        }

        else {

          if (result.nonEmpty && csvLineInArray.length != result.head.length)
            throw new Exception(s"Each line should contain the same number of fields throughout the file. Error on line $i")
          result.append(csvLineInArray.toVector)
        }
      }
      i += 1
    })
    return result.toList
  }

  def parse(line: String): ListBuffer[String] = {
    val res = new ListBuffer[String]
    val field = new mutable.StringBuilder
    var inQuotes = false
    var isThereInnerQuotes = false

    line.foreach(char => {
      if (char == doubleQuotes) {
        if (isThereInnerQuotes) {
          if (field.nonEmpty) {
            field.append(doubleQuotes)
            isThereInnerQuotes = false
          }
        }
        else {
          isThereInnerQuotes = true
        }
      }
      else {
        isThereInnerQuotes = false
      }
      if (isMultiline) {
        field.append(unclosedField).append(newLine)
        unclosedField = ""
        inQuotes = true
        isMultiline = false
      }
      if (char == doubleQuotes) {
        inQuotes = !inQuotes
      }
      else {
        if (char == separator && !(inQuotes)) {
          res.append(field.toString().trim)
          field.clear()
        } else {
          field.append(char)
        }
      }
    })
    if (inQuotes) {

      unclosedField = field.toString()
      isMultiline = true

    }
    else res.append(field.toString().trim)
    return res

  }
}