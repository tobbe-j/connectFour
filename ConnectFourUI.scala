package s1.connectfour
import o1._
import s1._

object ConnectFourUI extends App {

  val engine = new Engine(1, 550)
  val board = new GameBoard(1, 550)
  var isFinished = false

  object Connect {
    
    def playTurn(key: Key): Unit = { //spelar spelarens tur beroende på vad man trycker på och utför dropBlob för att placera en blob/bleb
      if (key == Key.Space || isFinished) {
        println("closing game")
        view.close()
      }

      val newCords = key match { //alla olika alternativ att klicka på som gör olika saker
        case Key.Key1 => engine.dropBlob(0)
        case Key.Key2 => engine.dropBlob(1)
        case Key.Key3 => engine.dropBlob(2)
        case Key.Key4 => engine.dropBlob(3)
        case Key.Key5 => engine.dropBlob(4)
        case Key.Key6 => engine.dropBlob(5)
        case Key.Key7 => engine.dropBlob(6)
        case Key.Key8 => engine.dropBlob(7)
        case Key.Key9 => engine.dropBlob(8)
        case Key.Key0 => engine.dropBlob(9)
        case _        => println("please press a number"); (-1, -1)
      }
      if (newCords != (-1, -1)) {
        board.drawNewTurn(engine.turn)
        if (engine.checkIfFour(newCords)) { //kollar vid slutet av en tur om man vunnit och rensar planen och ger poäng till vinnaren
          engine.clearBoard()
          board.createBoard(engine.turn)
          engine.addScore(engine.turn)
          board.drawScore(engine.playerOneScore, engine.playerTwoScore)
        }
      }
      //wincondition först till 5
      if (engine.playerOneScore == 5) {
        board.drawWinner(1)
        isFinished = true
      } else if (engine.playerTwoScore == 5) {
        board.drawWinner(2)
        isFinished = true
      }
      if (engine.winner == 1) {
        board.drawWinner(1)
        isFinished = true
      } else if (engine.winner == 2) {
        board.drawWinner(2)
        isFinished = true
      }
      
    }
  }

  val view = new s1.gui.mutable.View(Connect) {

    val game = model

    //ritar upp spelplanen 
    def makePic() = {
      for (y <- 0 until engine.boardSize; x <- 0 until engine.boardSize) {
        if (engine.gameBoard(y)(x) != 0) {
          board.drawCircle(x, y, engine.gameBoard(y)(x))
        }
      }

      board.background
    }

    override def onKeyUp(key: Key) = game.playTurn(key)
    

  }

  view.start()

}