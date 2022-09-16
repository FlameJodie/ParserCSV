import scala.io.Source
import scala.util.Try



object Main extends App{

  implicit class EitherSytax[+A, +B](e: Either[A,B]) {
    def leftMap[C](f: A => C): Either[C, B] = e match {
      case Right(x) => Right[C, B](x)
      case Left(x) => Left(f(x))
    }
  }

  trait Functor[F[_]] {
       def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  object Functor {
    def apply[F[_]](implicit f: Functor[F]): Functor[F] = f

    implicit class Syntax[F[_]: Functor, A](fa: F[A]) {
      def map[B](f: A => B): F[B] = Functor[F].map[A, B](fa)(f)
    }
  }
  import Functor.Syntax

  trait Monad[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  object Monad {
    def apply[F[_]](implicit f: Monad[F]): Monad[F] = f

    implicit class Syntax[F[_]: Monad, A](fa: F[A]) {
      def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
    }
  }
  import Monad.Syntax

  final case class EitherT[F[_]: Monad, A, B](value: F[Either[A, B]]) {
    def flatMap[C](f: B => EitherT[F[_], A, C]): EitherT[F, A, C] =

    EitherT(value.flatMap{
      case Right(b) => f(b).value
      case Left(e) => Monad[F].pure(Left(e))
    })
  }

 // def flatten[F[_], A](fa: F[F[A]]): F[A] = ???

 // def >>=[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(fmap(fa)(f))

  case class Foo[A](a: A) {
    def flatMap[B](f: A => Foo[B]): Foo[B] = >>=(this)(f)
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