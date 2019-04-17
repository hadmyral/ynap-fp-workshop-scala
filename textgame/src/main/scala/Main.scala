package textgame
import cats.effect.IO

object Main extends App {
  val program : IO[Unit] = new Game().run()
  program.unsafeRunSync
}
