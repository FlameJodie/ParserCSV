import org.scalatest.{Assertions, Ignore}
import org.scalatest.Assertions.assert
import org.scalatest.funsuite.AnyFunSuite

class CSVTest extends AnyFunSuite {
/*  test("FIXME") {
    val in = "FIXME"
    val expected = CSVParser.Csv(List())

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }*/

  test("empty csv") {
    val in = ""
    val expected = CSVParser.Csv(List())

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }

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

}