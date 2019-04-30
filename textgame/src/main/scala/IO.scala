package textgame

class IO[A](val run: () => A) {
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] (() => f(run()).run())

  def map[B](f: A => B): IO[B] =
    new IO[B] (() => f(run()))

  def *>[B](other: IO[B]): IO[B] =
    flatMap(_ => other)

  def attempt: IO[Either[Throwable, A]] =
    new IO[Either[Throwable, A]] (
      () => {
        try Right(run())
        catch {
          case e: Throwable => Left(e)
        }
      }
    )
}

object IO {
  def pure[A](a: A) : IO[A] = new IO[A](() => a)
  def unit() : IO[Unit] = new IO[Unit](() => ())
}