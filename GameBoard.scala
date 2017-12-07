package s1.connectfour
import o1._
import s1._

/**Representerar bilden som spelet ritar upp på skärmen
 *  @param startTurn - Vilken tur bilden i början visar att det är
 *  @param backGroundSize - storleken på spelområdet*/
class GameBoard(startTurn: Int, val backgroundSize: Int) {
  
  val vertLine = rectangle(1, 530, Black)
  val horizLine = rectangle(530, 1, Black)
  var background = rectangle(100,100,White)
  val blob = circle(40, Red) //den röda bollen för ena spelaren
  val bleb = circle(40, Blue) //den blåa bollen för andra spelaren
  this.createBoard(startTurn)  
  
  // används för att skapa spelplanen
  def createBoard(turn: Int) = {
  
    // här skapas planen
    val board = vertLine.onto(horizLine, TopCenter, new Pos(15, -15)).
                onto(horizLine, TopLeft, new Pos(0, -65)).
                onto(horizLine, TopLeft, new Pos(0, -115)).
                onto(horizLine, TopLeft, new Pos(0, -165)).
                onto(horizLine, TopLeft, new Pos(0, -215)).
                onto(horizLine, TopLeft, new Pos(0, -265)).
                onto(horizLine, TopLeft, new Pos(0, -315)).
                onto(horizLine, TopLeft, new Pos(0, -365)).
                onto(horizLine, TopLeft, new Pos(0, -415)).
                onto(horizLine, TopLeft, new Pos(0, -465)).
                onto(horizLine, TopLeft, new Pos(0, -515)).
                onto(vertLine, TopLeft, new Pos(-65, 0)).
                onto(vertLine, TopLeft, new Pos(-115, 0)).
                onto(vertLine, TopLeft, new Pos(-165, 0)).
                onto(vertLine, TopLeft, new Pos(-215, 0)).
                onto(vertLine, TopLeft, new Pos(-265, 0)).
                onto(vertLine, TopLeft, new Pos(-315, 0)).
                onto(vertLine, TopLeft, new Pos(-365, 0)).
                onto(vertLine, TopLeft, new Pos(-415, 0)).
                onto(vertLine, TopLeft, new Pos(-465, 0)).
                onto(vertLine, TopLeft, new Pos(-515, 0)).
                onto(rectangle(backgroundSize, backgroundSize, White), TopLeft, new Pos(10, 10))
  
    // här är siffrorna för kolumnerna
    val numberRow = rectangle(25, 100, White). 
                    onto(Pic("siffror/etta.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/tvåa.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/trea.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/fyra.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/femma.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/sexa.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/sjua.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/åtta.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/nia.png"),CenterRight, CenterLeft).
                    onto(Pic("siffror/nolla.png"),CenterRight, CenterLeft).
                    onto(rectangle(25, 100, White),CenterRight, CenterLeft)
                 
    //visar vems tur det är
    val playerTurn = if (turn == 1) Pic("siffror/Player1.png") else Pic("siffror/Player2.png")
    //här byggs sidopanelen
    val sidePanel = Pic("siffror/Turn.png").
                    onto(playerTurn, BottomCenter, TopCenter).
                    onto(Pic("siffror/Score.png"), BottomCenter, TopCenter).
                    onto(Pic("siffror/P1.png"), BottomCenter, TopCenter).
                    onto(Pic("siffror/P2.png"), BottomCenter, TopCenter).
                    onto(rectangle(200, 150, White), BottomCenter, TopCenter)

  
    background = board.onto(numberRow, BottomCenter, TopCenter).onto(sidePanel, TopLeft, TopRight)
  
    }
  
  //ritar en blob eller bleb på bakgrunden
  def drawCircle(xcord: Int, ycord: Int, player: Int) = {
    val step = (this.backgroundSize.toDouble / (10 + 1)).toInt
    if (player == 1) {
      background = blob.onto(background, new Pos(200 + (xcord + 1) * step, (ycord + 1) * step))
    } else {
      background = bleb.onto(background, new Pos(200 + (xcord + 1) * step, (ycord + 1) * step))
    }
  }
  
  //ritar ut poäng situationen för spelarna
  def drawScore(playerOne: Int, playerTwo: Int) = { 
    for (player <- 1 to 2) {
      val score = if (player == 1) playerOne else playerTwo
      var scorePic = Pic("siffror/nolla.png")
      scorePic = score match {
        case 1 => Pic("siffror/etta.png")
        case 2 => Pic("siffror/tvåa.png")
        case 3 => Pic("siffror/trea.png")
        case 4 => Pic("siffror/fyra.png")
        case 5 => Pic("siffror/femma.png")
        case _ => Pic("siffror/nolla.png")
      }
      if (player == 1) {
        background = scorePic.onto(background, TopLeft, new Pos(150, 300))
      } else {
      background = scorePic.onto(background, TopLeft, new Pos(150, 400))
      }

    }
    
  }
  
  //skriver vem som vann en runda
  def drawWinner(player: Int) = { 
    val winner = if (player == 1) Pic("siffror/Player1.png") else Pic("siffror/Player2.png")
    
    background = winner.onto(Pic("siffror/Winner.png"), Center, new Pos(375, 400))
  }
    
  //skriver vems tur det är
  def drawNewTurn(turn: Int) = {
    if (turn == 1) {
      background = Pic("siffror/Player1.png").onto(background, TopLeft , new Pos( 0, 100))
    } else {
      background = Pic("siffror/Player2.png").onto(background, TopLeft , new Pos( 0, 100))
    }

  }
}
