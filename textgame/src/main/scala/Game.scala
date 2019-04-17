package textgame

import scala.io.StdIn._
import scala.util.{Failure, Success, Try}

class Game {
  import Domain._
  import Logic._

  object Domain {
    case class Position(x: Int, y: Int)

    case class Player(name: String, position: Position)

    object Player {
      def begin(name: String) = Player(name, Position(0, 0))
    }

    case class Field(grid: Vector[Vector[String]])

    object Field {
      def mk20x20 =
        Field(Vector.fill(20, 20)("-"))
    }

    case class GameWorld(player: Player, field: Field) {
      def place(player: Player): Try[GameWorld] = {
        val size = field.grid.size - 1
        val position = player.position

        if (position.x < 0 || position.y < 0 || position.x > size || position.y > size)
          Failure(new Exception("Invalid direction"))
        else
          Success(this.copy(player = player))
      }
    }
  }

  sealed trait GameStatus
  case object Continue extends GameStatus
  case object Stop extends GameStatus

  sealed trait Command
  case object Help extends Command
  case object Show extends Command
  case object Quit extends Command
  case class Move(direction: Direction) extends Command
  case object NoDirection extends Command
  case object WrongMove extends Command
  case object UnknownCommand extends Command

  object Command {
    def apply(line: String): Command = {
      val strings = line.trim.toLowerCase.split("\\s+")
      strings(0) match {
        case "help" => Help
        case "show" => Show
        case "move" => if (strings.length < 2) NoDirection else Command.fromDirection(strings(1))
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
  case object Up extends Direction {
    override val x: Int = -1
    override val y: Int = 0
  }
  case object Down extends Direction {
    override val x: Int = 1
    override val y: Int = 0
  }
  case object Left extends Direction {
    override val x: Int = 0
    override val y: Int = -1
  }
  case object Right extends Direction {
    override val x: Int = 0
    override val y: Int = 1
  }

  sealed trait GameStepOutput

  case class Continue(world: GameWorld) extends GameStepOutput
  case class ContinueWithMessage(message: String) extends GameStepOutput
  case class ContinueWithError(message: String) extends GameStepOutput
  case class Stop(message: String) extends GameStepOutput

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

    sealed trait Line
    case class CommandLine(command: Command) extends Line
    case object NoCommandLine extends Line

    def gameLoop(world: GameWorld): Unit = {
      parseLine() match {
        case CommandLine(command) => executeCommand(command, world)
        case NoCommandLine => ()
      }
    }

    private def parseLine(): Line = {
      val line = readLine()
      line.length match {
        case 0 => NoCommandLine
        case _ => CommandLine(Command(line))
      }
    }

    private def executeCommand(command: Command, world: GameWorld) : Unit = {
      gameStep(command, world) match {
        case Continue(newWorld) =>
          gameLoop(newWorld)

        case ContinueWithMessage(message) =>
          println(message)
          gameLoop(world)

        case ContinueWithError(message) =>
          println(message)
          gameLoop(world)

        case Stop(message) =>
          println(message)
      }
    }

    def gameStep(command: Command, world: GameWorld): GameStepOutput = {
      command match {
        case Help => ContinueWithMessage(helpMessage())
        case Show => ContinueWithMessage(renderWorld(world))
        case NoDirection => ContinueWithMessage("Missing direction")
        case WrongMove => ContinueWithMessage("Unknown direction")
        case Move(direction) => move(world, direction)
        case Quit => Stop(s"Bye bye ${world.player.name}!")
        case UnknownCommand => ContinueWithMessage("Unknown command")
      }
    }

    def move(world: GameWorld, delta: Direction): GameStepOutput = {
      val player = world.player.copy(
        position = Position(
          world.player.position.x + delta.x,
          world.player.position.y + delta.y
        )
      )

      world.place(player)
        .fold(
          error => ContinueWithError(error.getMessage),
          newWorld => Continue(newWorld)
      )
    }

    def helpMessage(): String = {
        s"""|
            |Valid commands:
            |
            | help
            | show
            | move <up|down|left|right>
            | quit
            |""".stripMargin
    }

    def renderWorld(world: GameWorld): String = {
      val x       = world.player.position.x
      val y       = world.player.position.y
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
