package textgame

object Main extends App {
  val program : IO[Unit] = new Game().run()
  program.run()
}
