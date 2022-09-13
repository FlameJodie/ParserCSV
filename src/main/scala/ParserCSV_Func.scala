
import scala.annotation.tailrec
import scala.collection.mutable
//import scala.collection.IterableOnce.iterableOnceExtensionMethodsimport scala.collection.immutable
import scala.language.postfixOps

object CSVParser {

  final case class Error(msg: String, lineNumber: Int, pos: Int)

  final case class Csv(rows: List[Vector[String]])

  final case class Config(
                           fieldDelimiter: Char,
                           linesDelimiter: (Char, Option[Char]),
                           quote: Char
                         )

  object Config {
    def default: Config = Config(',', ('\n', None), '"')
  }

  def parseString(in: String, cfg: Config): Either[Error, Csv] = {
    parse(in.toCharArray.toList, cfg)
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
          case None => (true,tail)
        }
        case _ => (false, in)
      }
    }


    def parse0(in: List[Char], acc: List[List[Vector[Char]]]): Either[Error, List[List[Vector[Char]]]] = isEol(in) match {
      case (true, rest) => parse0(rest, Nil :: acc)
      case (false, current :: rest) => current match {
        case cfg.`fieldDelimiter` => acc match {
          case line :: lines => parse0(rest, (Vector()::line)::lines)
          case Nil => parse0(rest, List(List(Vector())))
        }
        case x => acc match {
          case line :: lines => line match {
             case field :: fileds => parse0(rest, List(field.appended(x)) :: lines)
             case Nil => parse0(rest, List(Vector(x)) :: lines)
          }
          case Nil => parse0( rest, List(List(Vector(x))))
        }
      }
      case (true, Nil) => Right(acc)
      case (false, Nil) => Right(acc)
    }

    parse0(in0, Nil).map(_.reverse.map(_.reverse.toVector.map(x => new String(x.toArray)))).map(Csv)
  }
}
