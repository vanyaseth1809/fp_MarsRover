import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit ={
    val gridUpperCorner = new Position(5,5,'N')
    val rover1 = new Position(1,2,'N')
    val roverMoves1 = "LMLMLMLMM"
    println(getFinalRoverWithReduce(roverMoves1.toList, rover1))

    val rover2 = new Position(3,3,'E')
    val roverMoves2 = "MMRMMRMRRM"
    println(getFinalRoverWithReduce(roverMoves2.toList, rover2))
  }

  case class Position(x: Int, y:Int, c:Char)

  def spinLeft(position: Position): Position = {
    position.c match {
      case 'N' => new Position(position.x,position.y,'W')
      case 'W' => new Position(position.x, position.y, 'S')
      case 'S' => new Position(position.x, position.y, 'E')
      case 'E' => new Position(position.x,position.y, 'N')
    }
  }

  def spinLeft2(position: Position) = position.copy(c = position.c match {
    case 'N' => 'W'
    case 'W' => 'S'
    case 'S' => 'E'
    case 'E' => 'N'
  })

  def spinRight(position: Position): Position = {
    position.c match {
      case 'N' => new Position(position.x,position.y,'E')
      case 'W' => new Position(position.x, position.y, 'N')
      case 'S' => new Position(position.x, position.y, 'W')
      case 'E' => new Position(position.x,position.y, 'S')
    }
  }

  def moveForward(position: Position): Position = {
    position.c match {
      case 'N' => new Position(position.x, position.y + 1, position.c)
      case 'S' => new Position(position.x, position.y - 1, position.c)
      case 'W' => new Position(position.x - 1, position.y, position.c)
      case 'E' => new Position(position.x + 1, position.y, position.c)
    }
  }

  @tailrec
  def getRoverFinalLocation(moves: List[Char], position: Position): Position = moves.headOption match {
    case None => position
    case Some(head) => {
      val currentNewPosition = moveRover(head,position)
      getRoverFinalLocation(moves.tail, currentNewPosition )
    }
  }

  def getFinalRoverWithReduce(moves: List[Char], position: Position) =
    moves.foldLeft(position){
      (pos,mov) => moveRover(mov,pos)
    }


  def moveRover(move: Char, currentPosition: Position): Position = {
    move match {
      case 'L' => spinLeft(currentPosition)
      case 'R' => spinRight(currentPosition)
      case 'M' => moveForward(currentPosition)
    }
  }
}