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
  case object Continue extends GameStatus
  case object Stop extends GameStatus

  sealed trait Command
  case object Help extends Command
  case object Show extends Command
  case object Quit extends Command
  case class Move(direction: Direction) extends Command
  case object NoMove extends Command
  case object WrongMove extends Command
  case object UnknownCommand extends Command

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

  trait GameStepOutput

  case class Continue(world: GameWorld) extends GameStepOutput
  case class ContinueWithMessage(message: String) extends GameStepOutput
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

        case Stop(message) =>
          println(message)
      }
    }

    def gameStep(command: Command, world: GameWorld): GameStepOutput = {
      command match {
        case Help => ContinueWithMessage(helpMessage())
        case Show => ContinueWithMessage(renderWorld(world))
        case NoMove => ContinueWithMessage("Missing direction")
        case WrongMove => ContinueWithMessage("Unknown direction")

        case Move(direction) =>
          try {
            Continue(move(world, direction))
          } catch {
            case e: Exception =>
              ContinueWithMessage(e.getMessage)
          }

        case Quit => Stop(s"Bye bye ${world.player.name}!")
        case UnknownCommand => ContinueWithMessage("Unknown command")
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
