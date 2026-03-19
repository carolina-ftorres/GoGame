import Main.{Board, Coord2D, History}
import Stone.Stone
case class Game(board: Board,
                player: Stone,
                lstOpenCoords: List[Coord2D],
                catchesToWin: (Int, Int),
                history: History) {

  override def toString: String ={
    val boardStr = writeBoard(board)
    val playerStr = player
    val catchesToWinStr = catchesToWin
    boardStr + playerStr + "\n" + catchesToWinStr
  }

  def writeBoard(board: Board): String = board match {
    case Nil => ""
    case head :: tail => writeLine(head) + "\n" + writeBoard(tail)
  }

  def writeLine(list: List[Stone]): String = list match {
    case Nil => ""
    case Stone.Black :: tail => "⚫" + writeLine(tail)
    case Stone.White :: tail => "⚪" + writeLine(tail)
    case Stone.Empty :: tail => "—" + writeLine(tail)
  }
}