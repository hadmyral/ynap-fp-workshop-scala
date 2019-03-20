package textgame

import scala.io.StdIn._

class Game {
  import Domain._
  import Logic._

  object Domain {

    case class Player(name: String, x: Int, y: Int)

    object Player {
      def begin(name: String) = Player(name, 0, 0)
    }

    case class Field(grid: Vector[Vector[String]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)("-"))
    }

    case class GameWorld(player: Player, field: Field)
  }

  sealed trait GameStatus
  object Continue extends GameStatus
  object Stop extends GameStatus

  sealed trait Command
  object Help extends Command
  object Show extends Command
  case class Move(direction: Option[Direction]) extends Command
  object Quit extends Command
  object UnknownCommand extends Command

  object Command {
    def apply(line: String): Command = {
      val strings = line.trim.toLowerCase.split("\\s+")
      strings(0) match {
        case "help" => Help
        case "show" => Show
        case "move" => if (strings.length < 2) Move(None) else Move(Some(Direction(strings(1))))
        case "quit" => Quit
        case _ => UnknownCommand
      }
    }
  }

  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction
  object UnknownDirection extends Direction

  object Direction {
    def apply(direction: String) : Direction = direction match {
      case "up" => Up
      case "down" => Down
      case "left" => Left
      case "right" => Right
      case _ => UnknownDirection
    }
  }

  object Logic {

    val enter: String = System.getProperty("line.separator")

    def initWorld(world: GameWorld): GameWorld = {
      println("Use commands to play")
      world
    }

    def askName(): String = {
      println("What is your name?")
      val name = readLine().trim
      println(s"Hello, $name, welcome to the game!")
      name
    }

    def gameLoop(world: GameWorld): Unit = {
      gameStep(world) match {
        case (Continue, newWorld) => gameLoop(newWorld)
        case (Stop, _) => ()
      }
    }

    def gameStep(world: GameWorld): (GameStatus, GameWorld) = {
      val line = readLine()

      if (line.length > 0) {
        val command = Command(line)
        command match {

          case Help =>
            printHelp()
            (Continue, world)

          case Show =>
            printWorld(world)
            (Continue, world)

          case Move(None) =>
            println("Missing direction")
            (Continue, world)

          case Move(Some(direction)) =>
            try {
              val newWorld = direction match {
                case Up    => move(world, (-1, 0))
                case Down  => move(world, (1, 0))
                case Right => move(world, (0, 1))
                case Left  => move(world, (0, -1))
                case _       =>
                  println("Unknown direction")
                  world
              }
              (Continue, newWorld)
            } catch {
              case e: Exception =>
                println(e.getMessage)
                (Continue, world)
            }

          case Quit =>
            printQuit(world)
            (Stop, world)

          case UnknownCommand =>
            println("Unknown command")
            (Continue, world)

        }
      } else {
        (Continue, world)
      }
    }

    def move(world: GameWorld, delta: (Int, Int)): GameWorld = {
      val newX = world.player.x + delta._1
      val newY = world.player.y + delta._2

      val size = world.field.grid.size - 1
      if (newX < 0
        || newY < 0
        || newX > size
        || newY > size) throw new Exception("Invalid direction")

      world.copy(
        player = world.player.copy(
          x = newX,
          y = newY)
      )
    }

    def printWorld(world: GameWorld): Unit =
      println(renderWorld(world))

    def printQuit(world: GameWorld): Unit =
      println(s"Bye bye ${world.player.name}!")

    def printHelp(): Unit = {
      val value =
        s"""|
            |Valid commands:
            |
            | help
            | show
            | move <up|down|left|right>
            | quit
            |""".stripMargin
      println(value)
    }

    def renderWorld(world: GameWorld): String = {
      val x       = world.player.x
      val y       = world.player.y
      val grid    = world.field.grid
      val updated = grid.updated(x, grid(x).updated(y, "x"))

      enter + updated.map(_.mkString(" ")).mkString(enter) + enter
    }
  }

  def run(): Unit = {
    val world = initWorld(GameWorld(Player.begin(askName()), Field.mk20x20))
    gameLoop(world)
  }
}
