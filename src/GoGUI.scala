import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class GoGUI extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("GoGame App")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("Initial.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()

    FxApp.scene = new Scene(mainViewRoot, 700, 500)

    primaryStage.setScene(FxApp.scene)
    primaryStage.setResizable(true)
    primaryStage.show()
  }
}

object FxApp {
  var scene: Scene = _
  var game: Game = _
  var file: String = _
  var lastPcMoveTime: Long = System.currentTimeMillis()
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[GoGUI], args: _*)
  }
}



