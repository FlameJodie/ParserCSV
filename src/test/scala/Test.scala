import CSVParser.{Csv, CsvWithCaptions}
import org.scalatest.{Assertions, Ignore}
import org.scalatest.Assertions.assert
import org.scalatest.funsuite.AnyFunSuite

class CSVTest extends AnyFunSuite {
  implicit class Escape(s: String) {
    def e: String = s"\"$s\""
  }
/*  test("FIXME") {
    val in = "FIXME"
    val expected = CSVParser.Csv(List())

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }*/

//  test("empty csv") {
//    val in = ""
//    val expected = CSVParser.Csv(List(Vector.empty))
//
//    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
//  }

  test("1line csv") {
    val in = "a,b,c"
    val expected = CSVParser.Csv(List(Vector("a","b","c")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
  test("2line csv") {
    val in = "a,b,c\nd,e,f"
    val expected = CSVParser.Csv(List(Vector("a","b","c"),Vector("d", "e", "f")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
  test("unquote csv") {
    val in = """aaa",b"bb,ccc"""
    val expected = CSVParser.Csv(List(Vector("aaa\"","b\"bb","ccc")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
  test("quote csv") {
    val in = """"a,b""\nc""""
    val expected = CSVParser.Csv(List(Vector("""a,b""\nc""")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
  test("empty fields") {
    val in = ",\n,"
    val expected = CSVParser.Csv(List(Vector("", ""), Vector("", "")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
  test(" csv") {

    val in = List(
      List("a", "a".e, "a"),
      List("b".e, "b", "b".e),
      List("c", "c".e, "c"),
    ).map(_.mkString(",")).mkString("\n\r")

    val expected = CSVParser.Csv(List(Vector("a", "a", "a"), Vector("b", "b", "b"), Vector("c", "c", "c")))

    assert(CSVParser.parseString(in, CSVParser.Config.default.copy(linesDelimiter = ('\n', Some('\r')))) == Right(expected))
  }
/*  test(" fields") {
    val in =  List(
      List("a", "\"a".e, "a")
    ).map(_.mkString(",")).mkString("\n\r")
    val expected = CSVParser.Csv(List(Vector("", ""), Vector("", "")))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Left(CSVParser.Error("",1,1)))
  }*/
  test("escape") {
    val in = List(
      List("aaa".e, "b \nbb".e, "ccc".e),
      List("zzz", "yyy", "xxx")
    ).map(_.mkString(",")).mkString("\n\r")

    val expected = CSVParser.Csv(List(Vector("aaa", "b \nbb", "ccc"), Vector("zzz", "yyy", "xxx")))

    assert(CSVParser.parseString(in, CSVParser.Config.default.copy(linesDelimiter = ('\n', Some('\r')))) == Right(expected))
  }


}