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
  object Quit extends Command
  case class Move(direction: Direction) extends Command
  object NoMove extends Command
  object WrongMove extends Command
  object UnknownCommand extends Command

  object Command {
    def apply(line: String): Command = {
      val strings = line.trim.toLowerCase.split("\\s+")
      strings(0) match {
        case "help" => Help
        case "show" => Show
        case "move" => if (strings.length < 2) NoMove else Command.fromDirection(strings(1))
        case "quit" => Quit
        case _ => UnknownCommand
      }
    }

    def fromDirection(direction: String) : Command = direction match {
      case "up" => Move(Up)
      case "down" => Move(Down)
      case "left" => Move(Left)
      case "right" => Move(Right)
      case _ => WrongMove
    }
  }

  sealed trait Direction {
    val x : Int
    val y : Int
  }
  object Up extends Direction {
    override val x: Int = -1
    override val y: Int = 0
  }
  object Down extends Direction {
    override val x: Int = 1
    override val y: Int = 0
  }
  object Left extends Direction {
    override val x: Int = 0
    override val y: Int = -1
  }
  object Right extends Direction {
    override val x: Int = 0
    override val y: Int = 1
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
        Command(line) match {
          case Help =>
            printHelp()
            (Continue, world)

          case Show =>
            printWorld(world)
            (Continue, world)

          case NoMove =>
            println("Missing direction")
            (Continue, world)

          case WrongMove =>
            println("Unknown direction")
            (Continue, world)

          case Move(direction) =>
            try {
              (Continue, move(world, direction))
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

    def move(world: GameWorld, delta: Direction): GameWorld = {
      val newX = world.player.x + delta.x
      val newY = world.player.y + delta.y

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
