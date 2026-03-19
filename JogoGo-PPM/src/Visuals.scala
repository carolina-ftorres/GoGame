import Stone.Stone
import Main.*

import java.io.PrintWriter
import scala.io.Source
import scala.io.StdIn.readLine


object Visuals {

  def printBoard(board: Board): String = board match{
    case Nil => " "
    case head :: tail =>
      println(printLine(head))
      printBoard(tail)
  }

  def printBoard(board: Option[Board]): String = board match {
    case None => "Não há nenhum board disponível :'("
    case Some(b) => printBoard(b)
  }

  def printLine(list: List[Stone]): String = list match {
    case Nil => ""
    case Stone.Black :: tail => "⚫ " + printLine(tail)
    case Stone.White :: tail => "⚪ " + printLine(tail)
    case Stone.Empty :: tail => " — " + printLine(tail)
  }

  def getInput(): String = readLine.trim.toUpperCase

  def getStone(): Stone = {
    println("Bem-vindo ao jogo")
    println("Escolha uma pedra para jogar (Black or White): ")
    println("A pedra preta começa")
    val player = getInput()
    player match {
      case "BLACK" => Stone.Black
      case "WHITE" => Stone.White
      case _ =>
        println("Escolha inválida :(")
        getStone()
    }
  }

  def getCoord(size: Int, board: Board): (Coord2D,Long) = {
    val initialTime = System.currentTimeMillis()
    println("Escolha a linha (x) onde quer jogar:")
    val input1 = getInput()
    val coordX = input1.toInt
    println("Escolha a coluna (y) onde quer jogar:")
    val input2 = getInput()
    val coordY = input2.toInt
    val finalTime = System.currentTimeMillis()
    val playTime = finalTime - initialTime
    if(coordX >= 0 && coordX < size && coordY >= 0 && coordY < size && board(coordX)(coordY) == Stone.Empty)
      ((coordX,coordY),playTime)
    else {
      println("Escolha inválida :(")
      getCoord(size,board)
    }
  }
  def getFile():String = {
    println("Quer salver onde")
    val file= getInput()
    file
  }

  def getBoardSize(): Int = {
    println("Escolha o tamanho do board:")
    val size = getInput()
    val boardSize = size.toInt
    if(boardSize <= 0)
      getBoardSize()
    else
      boardSize
  }

  def readSeed(fileName: String): Long = {
    val scanner = Source.fromFile(fileName)
    scanner.getLines().toList match {
      case line :: Nil =>
        scanner.close
        return line.toLong
      case _ =>
        scanner.close
        -1
    }
  }

  def writeSeed(fileName: String,seed:Long): Unit = {
    val printer = new PrintWriter(fileName)
    printer.print(seed)
    printer.close()
  }

  def fillBoard(size: Int): Board = {
    (0 until size).foldRight(List[List[Stone]]()) { (_,acc) =>
      val line = (0 until size).foldRight(List[Stone]()){
        (_,acc2) => Stone.Empty :: acc2
      }
      line :: acc
    }
  }

  def fillLstOpenCoords(size: Int): List[Coord2D] = {
    (0 until  size).foldRight(List[Coord2D]()){ (x,acc) =>
      val col = (0 until size).map(y => (x,y)).toList
      col ++ acc
    }
  }

  def getLevel(): Int ={
    println("Escolha o nível: ")
    println("easy, medium, hard")
    val level = getInput()
    level match {
      case "EASY" => 2
      case "MEDIUM" => 4
      case "HARD" => 6
      case _ =>
        println("Escolha inválida :)")
        getLevel()
    }
  }

  def startGame():Unit ={
    val level = getLevel()
    val boardSize = getBoardSize()
    val board = fillBoard(boardSize)
    val player = getStone()
    val opponent = if (player == Stone.Black) Stone.White else Stone.Black
    val lstOpencoords = fillLstOpenCoords(boardSize)
    val history = List[Game]()
    val points = (level,level)
    val file= getFile()// deve sair (salva sempre que fecha)
    if (player != Stone.Black) {
      val (newBoard,newLts,newPoints,newHistory) = game(board, opponent, (0,0) ,true,lstOpencoords,points,history)
      Visuals.printBoard(newBoard)
      turn(newBoard,player,opponent,newPoints,newLts,newHistory, file)
    }else {
      turn(board, player, opponent, points, lstOpencoords, history,file)
    }
  }

  def turn(board: Board, player: Stone, opponent: Stone, catchesToWin: (Int,Int),lst: List[Coord2D], history: History, file:String): Unit = {

    if(!win(catchesToWin,player,opponent)._1 && !endGame(lst)) {
      val (coord, time) = getCoord(board.head.length,board)
      if (time <= 30000) {
        val (newBoard, newLst, newcatchesToWin, newHistory) = game(board, player, coord, false, lst, catchesToWin, history)
        Visuals.printBoard(newBoard)
        val(cBoard,cLts,ccatchesToWin,cHistory)  = game(newBoard, opponent, (0,0), true, newLst, newcatchesToWin, newHistory)
        println("xxxxxxxxxxxxxxxxxx")
        Visuals.printBoard(cBoard)
        LoadSave.saveGame(file, cBoard, player, cLts, ccatchesToWin, cHistory)
        turn(cBoard,player,opponent,ccatchesToWin,cLts,cHistory,file)
      }
      val(cBoard,cLts,ccatchesToWin,cHistory)  = game(board, opponent, (0,0), true,lst,catchesToWin, history)
      Visuals.printBoard(cBoard)
      turn(cBoard,player,opponent,ccatchesToWin,cLts,cHistory,file)
    }else{

      if(win(catchesToWin,player,opponent)._2.get == player) {
        println("O jogador " + player + " ganhou :D")
        println(catchesToWin)
        sys.exit(0)
      }else if(win(catchesToWin,player,opponent)._2.get == opponent){
        println("O Pc ganhou :(")
        println(catchesToWin)
        sys.exit(0)
      }else{
        println("Empate :/")
        println(catchesToWin)
        sys.exit(0)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    Visuals.startGame()
  }
}