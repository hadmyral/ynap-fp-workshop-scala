package marsroverkata

import minitest._

object MarsRoverKataTests extends SimpleTestSuite {

  case class Position(x: Int, y: Int)
  case class Size(x: Int, y: Int)
  case class Rover(position: Position, direction: Direction)
  case class Planet(size: Size, rover: Rover)

  sealed trait Command
  case object F extends Command
  case object B extends Command
  case object L extends Command
  case object R extends Command

  sealed trait Direction {
    def turnRight: Direction = this match {
      case N => E
      case E => S
      case S => W
      case W => N
    }
  }
  case object N extends Direction
  case object E extends Direction
  case object W extends Direction
  case object S extends Direction

  def program(input: String, planet: Planet): Planet =
    parse(input)
      .foldLeft(planet) { (p, tc) =>
        tc.map(execute(p, _))
          .getOrElse(p)
      }

  def parse(value: String): List[Option[Command]] =
    value.toLowerCase.map(parse).toList

  def parse(c: Char): Option[Command] = c match {
    case 'f' => Some(F)
    case 'b' => Some(B)
    case 'l' => Some(L)
    case 'r' => Some(R)
    case _   => None
  }

  def execute(planet: Planet, cmd: Command): Planet = cmd match {
    case F => ???
    case B => ???
    case L => ???
    case R => turnRight(planet)
  }

  def turnRight(planet: Planet): Planet =
    planet.copy(rover = planet.rover.copy(direction = planet.rover.direction.turnRight))

  test("right rotation") {
    val p = Planet(Size(10, 10), Rover(Position(0, 0), N))
    assertEquals(program("r", p), Planet(Size(10, 10), Rover(Position(0, 0), E)))
    assertEquals(program("rr", p), Planet(Size(10, 10), Rover(Position(0, 0), S)))
    assertEquals(program("rrr", p), Planet(Size(10, 10), Rover(Position(0, 0), W)))
    assertEquals(program("rrrr", p), Planet(Size(10, 10), Rover(Position(0, 0), N)))
    assertEquals(program("rrrrr", p), Planet(Size(10, 10), Rover(Position(0, 0), E)))
    assertEquals(program("rxrrxrr", p), Planet(Size(10, 10), Rover(Position(0, 0), E)))
  }
}
