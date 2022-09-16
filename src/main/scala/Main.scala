import scala.io.Source
import scala.util.Try



object Main extends App{

  implicit class EitherSytax[+A, +B](e: Either[A,B]) {
    def leftMap[C](f: A => C): Either[C, B] = e match {
      case Right(x) => Right[C, B](x)
      case Left(x) => Left(f(x))
    }
  }

  def traverse[F[_], G[_], A, B](fa: F[A])(f: A => G[B]): G[F[A]] = ???

  implicit class ListSyntax[A,B](list:List[A]){
    def traverse(f: A => Either[String, B]): Either[String, List[B]] =
      list.foldRight[Either[String, List[B]]](Right(Nil)){
        case (a,acc) => f(a).flatMap(b => acc.map(b::_))
      }
  }

  import CSVParser._
  type Decoder[A] = RowWithCaption => Either[String, A]

  def decodeText(name: String): Decoder[String] = _.get(name)

  def decodeA[A](name: String, decoder: String => Either[String, A]): Decoder[A] = decodeText(name)(_).flatMap(decoder)

  def decodeInt(name: String): Decoder[Int] = decodeA[Int](name, x => Try(x.toInt).toEither.leftMap(_.getMessage))

  def decodeBoolean(name:String) :Decoder[Boolean] = decodeA[Boolean](name, {
    case "true" => Right(true)
    case "false" => Right(false)
    case y => Left(y)
  })

  case class FooBar(foo: Int, bar: Boolean)
  object FooBar {
    def decode: Decoder[FooBar] = row => for {
      x <- decodeInt("FOO")(row)
      y <- decodeBoolean("BAR")(row)
    } yield FooBar(x, y)
  }

  def program(in: String): Either[String, List[FooBar]] = {
    parseWithHeader(in,Config.default).leftMap(e => e.toString).flatMap(_.list.traverse(FooBar.decode))
  }

  val in = "FOO,BAR\n1,true\n2,false"

  program(in).fold(e => throw new RuntimeException(e), println(_))
}