import Main.{Board, endGame}
import Stone.Stone
import javafx.application.Application
import javafx.event.Event
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control.{Button, Label, ToggleGroup,TextField}
import javafx.scene.shape.Circle
import javafx.stage.Stage

class Controller {

  @FXML
  private var newGame: Button = _

  @FXML
  private var continueGame: Button = _

  @FXML
  private var close: Button = _

  @FXML
  private var difficulty: ToggleGroup = _

  @FXML
  private var playerColor: ToggleGroup = _

  @FXML
  private var returnButton: Button = _

  @FXML
  private var startGame: Button = _

  @FXML
  private var whiteCatchesLeft: Label = _

  @FXML
  private var blackCatchesLeft: Label = _

  @FXML
  private var winner: Label = _

  @FXML
  private var fileName: TextField = _

  @FXML
  private var loadFile: TextField = _

  @FXML
  private var undo: Button = _

  def onNewGameClicked(): Unit = {
    val loader = new FXMLLoader(getClass.getResource("Menu.fxml"))
    val menuRoot: Parent = loader.load()
    val stage = newGame.getScene.getWindow.asInstanceOf[Stage]
    FxApp.scene.setRoot(menuRoot)
    stage.setTitle("Menu Window")
  }

  def onCloseClicked(): Unit = {
    if (FxApp.file != null && FxApp.file.nonEmpty) {
      val game = FxApp.game
      LoadSave.saveGame(
        FxApp.file,
        game.board,
        game.player,
        game.lstOpenCoords,
        game.catchesToWin,
        List(game)
      )
    }
    System.exit(0)
  }

  def onContinueClicked(): Unit = {
    val loader = new FXMLLoader(getClass.getResource("File.fxml"))
    val gameRoot: Parent = loader.load()
    val stage = continueGame.getScene.getWindow.asInstanceOf[Stage]
    FxApp.scene.setRoot(gameRoot)
    stage.setTitle("Choose Game Window")
  }

  def onLoadClicked(): Unit = {
    val fileName = loadFile.getText().trim.toUpperCase

    LoadSave.loadGame(fileName) match {
      case Some(game) =>

        FxApp.game = game
        FxApp.file = fileName


        val loader = new FXMLLoader(getClass.getResource("Game.fxml"))
        val gameRoot: Parent = loader.load()
        val stage = loadFile.getScene.getWindow.asInstanceOf[Stage]
        FxApp.scene.setRoot(gameRoot)
        stage.setTitle("Game Window")

        updateBoardGUI(game.board)

        val blackLabel = gameRoot.lookup("#blackCatchesLeft").asInstanceOf[Label]
        val whiteLabel = gameRoot.lookup("#whiteCatchesLeft").asInstanceOf[Label]
        blackLabel.setText(s"Black Captures: " + game.catchesToWin._1)
        whiteLabel.setText(s"White Captures: " + game.catchesToWin._2)

        FxApp.game = Game(game.board,game.player,game.lstOpenCoords,game.catchesToWin,game.history)

      case None =>
        println(s"Failed to load game from " + fileName)
    }
  }

  def onUndoClicked(): Unit = {
    val game = FxApp.game

    if(game.history.nonEmpty){
      val (newGame,newHistory) = Main.undo(game.history)
      newGame match {
        case Some(newGame) =>
          FxApp.game = Game(newGame.board, game.player, newGame.lstOpenCoords, newGame.catchesToWin, newHistory)
          updateBoardGUI(newGame.board)
          val blackLabel = FxApp.scene.lookup("#blackCatchesLeft").asInstanceOf[Label]
          val whiteLabel = FxApp.scene.lookup("#whiteCatchesLeft").asInstanceOf[Label]
          blackLabel.setText(s"Black Captures: " + newGame.catchesToWin._1)
          whiteLabel.setText(s"White Captures: " + newGame.catchesToWin._2)
        case None =>
      }
    }
  }

  def onReturnClicked(): Unit ={
    val loader = new FXMLLoader(getClass.getResource("Initial.fxml"))
    val initialRoot: Parent = loader.load()
    val stage = returnButton.getScene.getWindow.asInstanceOf[Stage]
    FxApp.scene.setRoot(initialRoot)
    stage.setTitle("GoGame App")
  }

  def onStartGameClicked(): Unit = {
    val (difficultyValue, colorValue) = getSelectedOptions()
    FxApp.file = fileName.getText()

    val level = difficultyValue.toLowerCase match {
      case "easy" => 2
      case "medium" => 4
      case "hard" => 6
      case _ => 6
    }

    val player = colorValue.toLowerCase match {
      case "black" => Stone.Black
      case "white" => Stone.White
      case _ => Stone.Black
    }

    val boardSize = 5
    val board = Visuals.fillBoard(boardSize)
    val lstOpenCoords = Visuals.fillLstOpenCoords(boardSize)
    val catchesToWin = (level, level)

    val loader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val gameRoot: Parent = loader.load()
    val stage = startGame.getScene.getWindow.asInstanceOf[Stage]
    FxApp.scene.setRoot(gameRoot)
    stage.setTitle("Game Window")

    val blackLabel = gameRoot.lookup("#blackCatchesLeft").asInstanceOf[Label]
    val whiteLabel = gameRoot.lookup("#whiteCatchesLeft").asInstanceOf[Label]
    blackLabel.setText(s"Black Captures: " + catchesToWin._1)
    whiteLabel.setText(s"White Captures: " + catchesToWin._2)

    if (player == Stone.White) {
      FxApp.game = Game(board, Stone.Black, lstOpenCoords, catchesToWin,List())
      pcMove()
    } else {
      FxApp.game = Game(board, player, lstOpenCoords, catchesToWin,List())
    }

  }

  def getCoordFromWidgetID(id: String): (Int, Int) = {
    val parts = id.split("_")
    if (parts.length == 3) {
      val x = parts(1).toInt
      val y = parts(2).toInt
      (x, y)
    }else{
      (-1,-1)
    }
  }

  def playerMove(position: (Int, Int), startTime: Long): Boolean = {
    val currentTime = System.currentTimeMillis()
    if (currentTime - startTime > 30000) {
      val game = FxApp.game
      val opponent = if (game.player == Stone.Black) Stone.White else Stone.Black
      FxApp.game = Game(game.board, opponent, game.lstOpenCoords, game.catchesToWin, game :: game.history)
      pcMove()
      return false
    }

    val game = FxApp.game
    if (game.player != Stone.Black && game.player != Stone.White) return false

    val currentPlayer = game.player
    val opponent = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
    val board = game.board
    val lst = game.lstOpenCoords
    val catches = game.catchesToWin
    val history = List(game)

    if (board(position._1)(position._2) != Stone.Empty) return false

    val (newBoard, newLst, newCatches, newHistory) =
      Main.game(board, currentPlayer, position, false, lst, catches, history)

    FxApp.game = Game(newBoard, opponent, newLst, newCatches, game :: game.history)
    updateBoardGUI(newBoard)

    true
  }

  def pcMove(): Unit = {
    val game = FxApp.game
    val currentPlayer = game.player
    val opponent = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
    val board = game.board
    val lst = game.lstOpenCoords
    val catches = game.catchesToWin
    val history = List(game)

    val random = new scala.util.Random()
    val validCoords = lst.filter(coord => board(coord._1)(coord._2) == Stone.Empty)

    if (validCoords.nonEmpty) {
      val pos = validCoords(random.nextInt(validCoords.length))
      val (newBoard, newLst, newCatches, newHistory) =
        Main.game(board, currentPlayer, pos, true, lst, catches, history)

      FxApp.game = Game(newBoard, opponent, newLst, newCatches,game :: game.history)

      FxApp.lastPcMoveTime = System.currentTimeMillis()
      updateBoardGUI(newBoard)
    }
  }

  def checkWinner(): Unit = {
    val game = FxApp.game
    val (blackCatches, whiteCatches) = game.catchesToWin
    val currentPlayer = game.player
    val opponent = if (currentPlayer == Stone.Black) Stone.White else Stone.Black

    if (blackCatches <= 0 || whiteCatches <= 0) {
      val winner = if (blackCatches <= 0) Stone.Black else Stone.White
      showWinnerWindow(winner)
    }

    else if (endGame(game.lstOpenCoords)) {
      val winner = if (blackCatches < whiteCatches) Stone.Black
      else if (whiteCatches < blackCatches) Stone.White
      else Stone.Empty
      showWinnerWindow(winner)
    }
  }

  def showWinnerWindow(winner: Stone): Unit = {
    val loader = new FXMLLoader(getClass.getResource("Winner.fxml"))
    val gameRoot: Parent = loader.load()
    val winnerLabel = gameRoot.lookup("#winner").asInstanceOf[Label]
    FxApp.scene.setRoot(gameRoot)

    winner match {
      case Stone.Black => winnerLabel.setText("Black Player Wins!")
      case Stone.White => winnerLabel.setText("White Player Wins!")
      case Stone.Empty => winnerLabel.setText("Game Ended in a Tie!")
    }

    val stage = new Stage()
    stage.setScene(new Scene(gameRoot))
    stage.setTitle("Game Over")
    stage.show()
  }


  def updateBoardGUI(board: Board): Unit = {
    val (blackCatches, whiteCatches) = FxApp.game.catchesToWin

    val blackLabel = FxApp.scene.lookup("#blackCatchesLeft").asInstanceOf[Label]
    val whiteLabel = FxApp.scene.lookup("#whiteCatchesLeft").asInstanceOf[Label]

    blackLabel.setText(s"Black Captures:" + blackCatches)
    whiteLabel.setText(s"White Captures:" + whiteCatches)

    def updatePosition(x: Int, y: Int, stone: Stone): Unit = {
      val blackCircle = FxApp.scene.lookup(s"#b_${x}_${y}").asInstanceOf[Circle]
      val whiteCircle = FxApp.scene.lookup(s"#w_${x}_${y}").asInstanceOf[Circle]

      stone match {
        case Stone.Black =>
          blackCircle.setOpacity(1)
          whiteCircle.setOpacity(0)
        case Stone.White =>
          blackCircle.setOpacity(0)
          whiteCircle.setOpacity(1)
        case Stone.Empty =>
          blackCircle.setOpacity(0)
          whiteCircle.setOpacity(0)
      }
    }

    def processBoard(x: Int, y: Int): Unit = {
      if (x < board.length) {
        if (y < board(x).length) {
          updatePosition(x, y, board(x)(y))
          processBoard(x, y + 1)
        } else {
          processBoard(x + 1, 0)
        }
      }
    }

    processBoard(0, 0)
  }


  def onPlayClicked(event: Event): Unit = {
    val position = getCoordFromWidgetID(event.getSource.asInstanceOf[Circle].getId)
    if (position._1 == -1 || position._2 == -1) return

    val validMove = playerMove(position, FxApp.lastPcMoveTime)
    if (validMove) {
      pcMove()
      checkWinner()
    }
  }


  def getSelectedOptions(): (String, String) = {
    val selectedDifficultyToggle = difficulty.getSelectedToggle
    val difficultyValue = if (selectedDifficultyToggle != null)
      selectedDifficultyToggle.asInstanceOf[javafx.scene.control.ToggleButton].getText
    else
      "No difficulty selected"

    val selectedColorToggle = playerColor.getSelectedToggle
    val colorValue = if (selectedColorToggle != null)
      selectedColorToggle.asInstanceOf[javafx.scene.control.ToggleButton].getText
    else
      "No color selected"

    (difficultyValue, colorValue)
  }


}

