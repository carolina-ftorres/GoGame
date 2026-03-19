import Stone.Stone
import Visuals.{readSeed, writeSeed}

import scala.util.Random

object Main {

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)
  type History = List[Game]

  // ---------------------------------------------------- Jogadas

  //T1
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (index, nextRand) = rand.nextInt(lstOpenCoords.size)
    val coord = lstOpenCoords(index)
    writeSeed("seeds",nextRand.seed)
    if (hasLiberties(coord, lstOpenCoords) && lstOpenCoords.contains(coord)) (coord, nextRand)
    else randomMove(lstOpenCoords, nextRand)
  }

  //T2
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    val (x, y) = coord
    if (isInsideBounds(board, coord) && board(x)(y) == Stone.Empty) {
      val newBoard = copyBoard(board, coord, player, 0)
      val newLstOpenCoords = lstOpenCoords.filter(_ != coord)
      (Some(newBoard), newLstOpenCoords)
    } else {
      (Some(board), lstOpenCoords) //MUDEI NONE PARA BOARD
    }
  }

  def capture(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Board, List[Coord2D], Int) = {
    val (boardCapt, lstCoords, numberCapt) = if (board.isEmpty) {
      (board, lstOpenCoords, 0)
    } else {
      val opponent = if (player == Stone.Black) Stone.White else Stone.Black
      val (boardAfterCapture, numCapt) = captureGroupStones(board, opponent)

      if (hasLiberties(coord, lstOpenCoords) | boardAfterCapture != board) {
        (boardAfterCapture, lstOpenCoords, numCapt)
      } else {
        (board, lstOpenCoords, 0)
      }
    }
    (boardCapt, lstCoords, numberCapt)
  }

  //T3
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {
    val newPlay = play(board, player, f(lstOpenCoords, r)._1, lstOpenCoords)
    (newPlay._1.get, f(lstOpenCoords, r)._2, newPlay._2)
  }

  // ----------------------------------------------------- Auxiliares

  //verifica se o movimento é válido (tem liberdades)
  def hasLiberties(coord: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    if (!lstOpenCoords.contains((coord._1 + 1, coord._2)) &&
      !lstOpenCoords.contains((coord._1 - 1, coord._2)) &&
      !lstOpenCoords.contains((coord._1, coord._2 + 1)) &&
      !lstOpenCoords.contains((coord._1, coord._2 - 1)))
      false
    else
      true
  }

  def isInsideBounds(board: Board, coord: Coord2D): Boolean = {
    coord._1 >= 0 && coord._1 < board.length && coord._2 >= 0 && coord._2 < board.head.length
  }

  //copiadores da matriz board
  def copyBoard(board: Board, coord: Coord2D, player: Stone, iterator: Int): Board = board match {
    case Nil => Nil
    case x :: xs => if (coord._1 == iterator) {
      def copyRow(linha: List[Stone], coordY: Int, player: Stone, iterator: Int): List[Stone] = linha match {
        case Nil => Nil
        case x :: xs => if (coordY == iterator) player :: xs
        else x :: copyRow(xs, coordY, player, iterator + 1)
      }

      copyRow(x, coord._2, player, 0) :: xs
    }
    else
      x :: copyBoard(xs, coord, player, iterator + 1)
  }

  //----------------------------------------------------- Métodos de jogo

  //T5
  def captureGroupStones(board: Board, opponent: Stone): (Board, Int) = {
    def groupBy(toVisit: List[Coord2D], visited: List[Coord2D]): List[Coord2D] = toVisit match {
      case Nil => visited
      case (x, y) :: rest =>
        if (!visited.contains((x, y)) && isInsideBounds(board, (x, y)) && board(x)(y) == opponent) {
          val neighbors = List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
          groupBy(neighbors ++ rest, (x, y) :: visited)
        } else {
          groupBy(rest, visited)
        }
    }

    def findGroups(x: Int, y: Int, groups: List[List[Coord2D]], visitedGroups: List[Coord2D]): (List[List[Coord2D]], List[Coord2D]) = { //adicionar
      if (x >= board.length) {
        (groups, visitedGroups)
      } else if (y >= board.head.length) {
        findGroups(x + 1, 0, groups, visitedGroups)
      } else if (board(x)(y) == opponent && !visitedGroups.contains((x, y))) {
        val group = groupBy(List((x, y)), List())
        findGroups(x, y + 1, group :: groups, visitedGroups ++ group)
      } else {
        findGroups(x, y + 1, groups, visitedGroups)
      }
    }

    def groupIterator(groups: List[List[Coord2D]], board: Board): (Board, Int) = groups match {
      case Nil => (board, 0)
      case currentGroup :: restGroups =>
        if (isCaptured(board, currentGroup)) {
          val (updatedBoard, count) = deleteGroup(board, currentGroup, 0)
          val (finalBoard, totalCaptured) = groupIterator(restGroups,updatedBoard)
          (finalBoard, totalCaptured + count)
        } else {
          groupIterator(restGroups,board)
        }
    }

    val (groups, _) = findGroups(0, 0, List(), List())
    if (groups.isEmpty) return (board, 0)
    val (finalBoard,totalCaptured) = groupIterator(groups,board)
    (finalBoard, totalCaptured)
  }

  def isCaptured(board: Board, groupCoords: List[Coord2D]): Boolean = groupCoords match {
    case Nil => true
    case (x, y) :: xs =>
      val validCoords = List(
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1)
      ).filter(isInsideBounds(board, _))
      if (validCoords.exists { case (x, y) => board(x)(y) == Stone.Empty }) false
      else isCaptured(board, xs)
  }

  def deleteGroup(board: Board, groupCoords: List[Coord2D], NumCapture: Int): (Board, Int) = groupCoords match {
    case Nil => (board, NumCapture)
    case x :: xs => deleteGroup(copyBoard(board, x, Stone.Empty, 0), xs, NumCapture + 1)
  }
  //PRIMEIRO BLACK SEGUNDO WHITE
  def win(numberOfCatches: (Int, Int), player: Stone, opponent: Stone): (Boolean, Option[Stone]) = {
    val (blackCatches, whiteCatches) = numberOfCatches
    if (blackCatches <= 0 && player == Stone.Black) {
      //println("O jogador humano preto graanho ")
      (true, Some(player))
    } else if (whiteCatches <= 0 && player == Stone.White) {
      //println("O jogador humano branco graanho ")
      (true, Some(player))
    } else if (blackCatches <= 0 && opponent == Stone.Black) {
      //println("O jogador pc preto graanho ")
      (true, Some(opponent))
    } else if (whiteCatches <= 0 && opponent == Stone.White) {
      //println("O jogador pc branco graanho ")
      (true, Some(opponent))
    } else {
      //println("o jogo continua ")
      (false, None)

    }
  }



  def endGame(lstOpenCoords: List[Coord2D]): Boolean = lstOpenCoords match {
    case Nil => true
    case x :: xs => !hasLiberties(x,lstOpenCoords) && endGame(xs)
  }

  def game(board: Board, player: Stone, coord: Coord2D, isRandom: Boolean, lstOpenCoords: List[Coord2D], catchesToWin: (Int, Int), history: History): (Board, List[Coord2D], (Int, Int), History) = {
    val game = Game(board, player, lstOpenCoords, catchesToWin,history)
    val newHistory = saveGame(history, game)
    val myRandom = MyRandom(readSeed("seeds"))

    val (newBoard, newLstOpenCoords) =
      if (!isRandom)
        play(board, player, coord, lstOpenCoords)
      else {
        val (newRandomBoard, _, newLstRandom) = playRandomly(board, myRandom, player, lstOpenCoords, randomMove)
        (Some(newRandomBoard), newLstRandom)
      }

    val (boardAfterRandom, lstAfterRandom, numberOfCatchesRandom) = capture(newBoard.get, player, coord, newLstOpenCoords)
    if (player == Stone.Black) (boardAfterRandom, lstAfterRandom, (catchesToWin._1 - numberOfCatchesRandom, catchesToWin._2), newHistory)
    else (boardAfterRandom, lstAfterRandom, (catchesToWin._1, catchesToWin._2 - numberOfCatchesRandom), newHistory)

  }

  def saveGame(history: History, game: Game): (History) = {
    (game :: history)
  }

  def undo(history: History): (Option[Game], History) = history match {
    case Nil => (None, Nil)
    //case x :: Nil => (Some(x), Nil) //apenas para testes na consola quando a jogada do pc não é instantânea
    case x :: y :: xs => (Some(y), xs)
  }

  def main(args: Array[String]): Unit = {
    val listTest: List[Coord2D] = List((0, 1), (0, 2), (1, 3),(1,4),(2,1),(2,4), (3, 3), (3, 4), (4, 0), (4, 2), (4, 3), (4, 4))
    val r: MyRandom = MyRandom(1)

    val history: History = List()
    val board: Board = List(
      List(Stone.White, Stone.Empty, Stone.Empty, Stone.White, Stone.Black),
      List(Stone.Black, Stone.White, Stone.White, Stone.Empty, Stone.Empty),
      List(Stone.Black, Stone.Empty, Stone.Black, Stone.White, Stone.Empty),
      List(Stone.White, Stone.Black, Stone.White, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.White, Stone.Empty, Stone.Empty, Stone.Empty)
    )



    //    val board: Board = List(
    //      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
    //      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
    //      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
    //      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
    //      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty)
    //    )

    //    Visuals.printBoard(board)
    //    println("....Jogada 1...")
    //    val (newBoard, newList, n, h) = game(board, Stone.White, (1,4), false, listTest, (5, 5), history)
    //    Visuals.printBoard(newBoard)
    //    println(n)
    //    println(h)
    //    println("...Jogada 2....")
    //    val (newBoard2, newList2, n2, h2) = game(newBoard, Stone.Black, (2, 1),true, newList, n, h)
    //    Visuals.printBoard(newBoard2)
    //    println(n2)
    //    println(h2)
    //    println("...Jogada 3....")
    //    val (newBoard3, newList3, n3, h3) = game(newBoard2, Stone.Black, (2, 1), true, newList2, n2, h2)
    //    Visuals.printBoard(newBoard3)
    //    println(n3)
    //    println(h3)

    //    val (g, hgay1) = undo(h)
    //    Visuals.printBoard(g.get.board)
    //    println(g.get.catchesToWin)
    //    println("...Jogada 2....")
    //    val (newBoard2, bool2, newList2, n2, h2) = game(newBoard, Stone.Black, (0, 4), newList, n, h)
    //    Visuals.printBoard(newBoard2)
    //    println(n2)
    //    println(h2)
    //    println("...Jogada 3....")
    //    val (newBoard3, bool3, newList3, n3, h3) = game(newBoard2, Stone.White, (0, 3), newList2, n2, h2)
    //    Visuals.printBoard(newBoard3)
    //    println(n3)
    //    println("...Jogada 4....")
    //    val (newBoard4, bool4, newList4, n4, h4) = game(newBoard3, Stone.White, (1, 4), newList3, n3, h3)
    //    Visuals.printBoard(newBoard4)
    //    println(n4)
    //    println(h4)


  }


  //        //        println("....Random........")
  //        //        val (newBoard2, newRand, newList2) = playRandomly(newBoard.get, r, Stone.Black, newList, randomMove)
  //        //        Visuals.printBoard(newBoard2)
  //        println("....Jogada3........")
  //        val (newBoard3, newList3) = play(newBoard.get, Stone.White, (3, 2), newList1)
  //        Visuals.printBoard(newBoard3)
  //        println("....Jogada4........")
  //        val (newBoard4, newList4) = play(newBoard3.get, Stone.White, (4, 1), newList3)
  //        Visuals.printBoard(newBoard4)
  //        println("....Jogada5........")
  //        val (newBoard5, newList5) = play(newBoard4.get, Stone.White, (2, 0), newList4)
  //        Visuals.printBoard(newBoard5)
  //        println("....Jogada6........")
  //        val (newBoard6, newList6) = play(newBoard5.get, Stone.White, (0, 0), newList5)
  //        Visuals.printBoard(newBoard6)
  //
  //    println("....Jogada1...")
  //    val (newBoard,win,numberCatches) = game("easy",board, Stone.White, (1, 0), listTest,0)
  //    Visuals.printBoard(newBoard)
  //    println("....Jogada3........")
  //    val (newBoard3, newList3,numberCatches3) = game("easy",newBoard, Stone.White, (1, 0), listTest,numberCatches)
  //    Visuals.printBoard(newBoard3)
  //    println("....Jogada4........")
  //    val (newBoard4, newList4,numberCatches4) = game("easy",newBoard, Stone.White, (1, 0), listTest,numberCatches)
  //    Visuals.printBoard(newBoard4)
  //    println("....Jogada5........")
  //    val (newBoard5, newList5,numberCatches5) = game("easy",newBoard, Stone.White, (1, 0), listTest,numberCatches)
  //    Visuals.printBoard(newBoard5)
  //    println("....Jogada6........")
  //    val (newBoard6, newList6,numberCatches6) = game("easy",newBoard, Stone.White, (1, 0), listTest,numberCatches)
  //    Visuals.printBoard(newBoard6)
}