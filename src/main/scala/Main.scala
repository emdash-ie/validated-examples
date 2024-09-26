import cats.Applicative
import cats.data.Validated._
import cats.data.ValidatedNec
import cats.effect.{IO, IOApp}
import cats.syntax.either._
import cats.syntax.traverse._

object Main extends IOApp.Simple {
  val run = for {
    v <- getInts()
    _ <- v match {
      case Valid(ns) => IO.println(s"Well done! You gave me these numbers: ${ns}")
      case Invalid(es) => for {
        _ <- IO.println("Hmm, there were some problems with those numbers:")
        _ <- es.traverse(e => IO.println(s"- ${e}"))
      } yield ()
    }
  } yield ()

  def getInts(): IO[ValidatedNec[String, Seq[Int]]] = {
    Applicative[IO]
      .compose[[A] =>> ValidatedNec[String, A]]
      .map4(
        getInt(),
        getInt(),
        getInt(),
        getInt(),
      )((x, y, z, n) => Seq(x, y, z, n))
  }

  def getInt(): IO[ValidatedNec[String, Int]] = for {
    response <- prompt("Please enter a number: ")
  } yield (response.toIntOption.toRight(s"${response} is not a number").toValidatedNec)

  case class UserData(name: String, emailAddress: String, favouriteNumber: Int)

  sealed trait UserError
  case class DisallowedNumber(n: Int) extends UserError

  def getDetails(): IO[ValidatedNec[UserError, UserData]] = for {
    responseLines <- IO.prompt("Enter your name, email address, and favourite number on separate lines:\n", 3)
    responseLines match {
      case Seq(name, email, number) =>
        Applicative[[A] =>> ValidatedNec[UserError, UserData]].map3(
          validateName(name),
          validateEmail(email),
          validateNumber(number),
        )(UserData)
    }
  }

  def validateName(name: String): ValidatedNec[UserError, String] = name.valid

  def validateEmail(email: String): ValidatedNec[UserError, String] = email.valid

  def validateNumber(n: Int): ValidatedNec[UserError, Int] = n match {
    case 7 => 7.valid
    case _ => DisallowedNumber(n).invalid
  }


  def prompt(query: String, numberOfLines: Int): IO[Seq[String]] = for {
    _ <- IO.print(query)
    response <- IO.readLine.replicate(numberOfLines)
  } yield (response)
}
