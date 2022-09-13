import org.scalatest.Assertions
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
    val expected = CSVParser.Csv(List(Vector.empty))

    assert(CSVParser.parseString(in, CSVParser.Config.default) == Right(expected))
  }
}