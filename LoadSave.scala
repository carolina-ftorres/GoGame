import Stone.Stone
import Main.*
import java.io._
import scala.io.Source
import scala.io.StdIn.readLine

object LoadSave {
  def saveGame(file: String, board: Board, player: Stone, lstOpenCoords: List[Coord2D], catchesToWin: (Int,Int), history: History): Unit = {
    val game = Game(board, player, lstOpenCoords, catchesToWin, history)
    try {
      val writer = new PrintWriter(new File(file))
      writer.println(game.toString)
      writer.close()
    } catch {
      case e: FileNotFoundException =>
        println("Error: File not found.")
      case e: IOException =>
        println("Error writing to file.")
    }
  }


  def loadGame(file: String): Option[Game] = {
    try {
      val lines = Source.fromFile(file).getLines().toList

      val boardEndIndex = lines.indexWhere { line =>
        !line.exists(c => c == '⚫' || c == '⚪' || c == '—')
      }

      val boardLines = lines.slice(0, boardEndIndex)
      val rest = lines.drop(boardEndIndex)

      val board = boardLines.map(_.trim.toCharArray.toList.map {
        case '⚫' => Stone.Black
        case '⚪' => Stone.White
        case _   => Stone.Empty
      })

      val player = rest(0).trim match {
        case "Black" => Stone.Black
        case "White" => Stone.White
        case _                       => Stone.Empty
      }


      val lstOpenCoords: List[Coord2D] = {
        def loop(y: Int, rows: List[List[Stone]], acc: List[Coord2D]): List[Coord2D] = rows match {
          case Nil => acc
          case row :: rest =>
            def loopRow(x: Int, stones: List[Stone], innerAcc: List[Coord2D]): List[Coord2D] = stones match {
              case Nil => innerAcc
              case Stone.Empty :: xs => loopRow(x + 1, xs, (x, y) :: innerAcc)
              case _ :: xs => loopRow(x + 1, xs, innerAcc)
            }

            loop(y + 1, rest, loopRow(0, row, acc))
        }

        loop(0, board, Nil).reverse
      }

      val catchLine = rest(1)
      val catchStart = catchLine.indexOf('(')
      val catchEnd = catchLine.indexOf(')')
      val catchRaw = catchLine.substring(catchStart + 1, catchEnd)
      val catchParts = catchRaw.split(",").map(_.trim.toInt)
      val catchesToWin = (catchParts(0), catchParts(1))

      Some(Game(board, player, lstOpenCoords, catchesToWin, List()))
    } catch {
      case e: Exception =>
        println("Erro ao carregar o jogo: ")
        None
    }
  }

}