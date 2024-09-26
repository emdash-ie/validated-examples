import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  val run = IO.println("Hello, World!")

  def prompt(query: String): IO[String] = for {
    _ <- IO.println(query)
  } yield ("hello")
}
