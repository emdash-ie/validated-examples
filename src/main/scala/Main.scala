import cats.Applicative
import cats.data.Validated._
import cats.data.ValidatedNec
import cats.effect.{IO, IOApp}
import cats.syntax.either._
import cats.syntax.traverse._
import cats.syntax.validated._

case class UserData(name: String, emailAddress: UserEmail, favouriteNumber: Int)
case class UserEmail(user: String, host: String)

sealed trait UserError
case class NotANumber(n: String) extends UserError
case class DisallowedNumber(n: Int) extends UserError
case class InvalidEmail(email: String) extends UserError

object Main extends IOApp.Simple {
  val run = for {
    userData <- getDetails()
    _ <- userData match {
      case Valid(ud) => IO.println(s"\nThat all looks ok! ${ud}")
      case Invalid(es) => for {
        _ <- IO.println("\nUh oh, there were some problems with your data:")
        _ <- es.traverse((e) => IO.println(s"- ${e}"))
      } yield ()
    }
  } yield ()

  def getDetails(): IO[ValidatedNec[UserError, UserData]] = for {
    responseLines <- prompt("Enter your name, email address, and favourite number on separate lines:\n\n", 3)
  } yield {
    responseLines match {
      case Seq(name, email, number) =>
        Applicative[[A] =>> ValidatedNec[UserError, A]].map3(
          validateName(name),
          validateEmail(email),
          validateNumber(number),
        )(UserData.apply)
    }
  }

  def validateName(name: String): ValidatedNec[UserError, String] = name.validNec

  def validateEmail(email: String): ValidatedNec[UserError, UserEmail] = email.split('@') match {
    case Array(user, host) => UserEmail(user, host).validNec
    case _ => InvalidEmail(email).invalidNec
  }

  def validateNumber(number: String): ValidatedNec[UserError, Int] = (for {
    n <- number.toIntOption.toRight(NotANumber(number))
    r <- n match {
      case 7 => Right(7)
      case _ => Left(DisallowedNumber(n))
    }
  } yield r).toValidatedNec


  def prompt(query: String, numberOfLines: Int): IO[Seq[String]] = for {
    _ <- IO.print(query)
    response <- IO.readLine.replicateA(numberOfLines)
  } yield (response)
}
