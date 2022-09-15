
import scala.:+
import scala.annotation.tailrec
import scala.collection.{Map, mutable}
//import scala.collection.IterableOnce.iterableOnceExtensionMethodsimport scala.collection.immutable
import scala.language.postfixOps

object CSVParser {

  final case class Error(msg: String, lineNumber: Int, pos: Int)

  final case class Csv(
                        rows: List[Vector[String]]
                      )

  final case class Config(
                           fieldDelimiter: Char,
                           linesDelimiter: (Char, Option[Char]),
                           quote: Char
                         ) {
    def linesDelimiterLength(): Int = {
      linesDelimiter._2 match {
        case Some(_) => 2
        case None => 1
      }
    }
  }


  final case class CsvWithCaptions(captions: Map[String, Int], csv: Csv)


  implicit class finder(csvWithCaptions: CsvWithCaptions) {

    def getByName(line: Int, name: String): Either[String, String] = {
      csvWithCaptions.captions.get(name) match {
        case Some(n) =>
          Right(csvWithCaptions.csv.rows match {
            case List() => return Left("looks like there are only headers")
            case list => list(line)(n)

          })
        case None => Left(s"there is no column $name")
      }

    }
  }

  object Config {
    def default: Config = Config(',', ('\n', None), '"')
  }

  def parseString(in: String, cfg: Config): Either[Error, Csv] = {
    parse(in.toCharArray.toList, cfg)
  }

  def parseWithHeader(in:String, cfg: Config): Either[Error, CsvWithCaptions] = {
    parse(in.toCharArray.toList, cfg) match {
      case Right(list) => list.rows match {
        case x :: tail => Right(CsvWithCaptions(x.zipWithIndex.toMap, Csv(tail)))
        case Nil => Left(Error("empty csv", 0, 0))
      }
      case Left(err) => Left(err)
    }
     }

  def parse(in0: List[Char], cfg: Config): Either[Error, Csv] = {
    def isEol(in: List[Char]): (Boolean, List[Char]) = {
      in match {
        case cfg.linesDelimiter.`_1` :: tail => cfg.linesDelimiter._2 match {
          case Some(delim2) => tail match {
            case x :: tail0 if (x == delim2) => (true, tail0)
            case Nil => (true, Nil)
            case _ => (false, in)
          }
          case None => (true, tail)
        }
        case _ => (false, in)
      }
    }

    @tailrec
    def parseEscaped(in: List[Char], acc: List[List[Vector[Char]]], pos: Int): Either[Error, List[List[Vector[Char]]]] = {
      in match {
        case cfg.`quote` :: rest => rest match {
          case cfg.`quote` :: rest0 => parseEscaped(rest0, addToAcc(cfg.quote, addToAcc(cfg.quote, acc)), pos + 2)
          case cfg.`fieldDelimiter` :: _ => parse0(rest, acc, false, pos + 2)
          case _ if isEol(rest)._1 => parse0(rest, acc, false, pos + 2)
          case Nil => parse0(rest, acc, false, pos + 2)
          case x :: _ => Left(Error(s"unexpected symbol $x", acc.length, pos + 2))
        }
        case Nil => Left(Error(s"unexpected end of input", acc.length, pos))
        case x :: rest => parseEscaped(rest, addToAcc(x, acc), pos)
      }

    }

    def addToAcc(x: Char, acc: List[List[Vector[Char]]]): List[List[Vector[Char]]] = acc match {
      case line :: lines => line match {
        case field :: fields =>
          (field.appended(x) :: fields) :: lines
        case Nil => List(Vector(x)) :: lines
      }
      case Nil => List(List(Vector(x)))
    }

    def addEmptyField(acc: List[List[Vector[Char]]]): List[List[Vector[Char]]] = acc match {
      case line :: lines
      => (Vector() :: line) :: lines
      case Nil => List(List(Vector()))
    }

    @tailrec
    def parse0(in: List[Char], acc: List[List[Vector[Char]]], isNewField: Boolean, pos: Int): Either[Error, List[List[Vector[Char]]]] = isEol(in) match {
      case (true, rest) => parse0(rest, addEmptyField(Nil :: acc), true, pos + cfg.linesDelimiterLength)
      case (false, current :: rest) => current match {
        case cfg.`quote` if isNewField => parseEscaped(rest, acc, pos + 1)
        case cfg.`fieldDelimiter` => parse0(rest, addEmptyField(acc), true, pos + 1)

        case x => parse0(rest, addToAcc(x, acc), false, pos + 1)
      }
      case (true, Nil) => if (isNewField) Right(addEmptyField(acc)) else Right(acc)
      case (false, Nil) => Right(acc)
    }


    parse0(in0, List(List(Vector.empty)), true, 0).map(_.reverse.map(_.reverse.toVector.map(x => new String(x.toArray)))).map(Csv)


  }
}