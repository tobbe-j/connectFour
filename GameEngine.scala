package s1.connectfour
import o1._
import s1._
import scala.collection.mutable.Buffer

/**
 * @param startTurn           vems tur det är att börja
 * @param boardSize           hur stor board det är man spelar med både y-led och x-led
 * @param backgroundSize      bakgrundsstorleken för spelplanen
 */



class Engine(startTurn: Int, val backgroundSize: Int) {
  
  val boardSize = 10
  var playerOneScore = 0
  var playerTwoScore = 0
  private var gameboard = Array.ofDim[Int](boardSize, boardSize)
  private var playerTurn = startTurn
  private val directions = Vector((0,0),(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)) //olika hållen att kolla
  
  def turn = playerTurn
  
  def gameBoard = gameboard
  
  def winner = if (playerOneScore == 5) 1 else if (playerTwoScore == 5) 2 else 0
  
  //ger poäng åt vinnande spelaren som fått 4 i rad
  def addScore(player: Int) = if (player == 2) playerOneScore = playerOneScore + 1 else playerTwoScore = playerTwoScore + 1
  
  def clearBoard() = gameboard = Array.ofDim[Int](boardSize, boardSize)
  
  //placerar en "blob" i en given kolumn och ger turen vidare till nästa spelare
  def dropBlob(column: Int) = { 
    var newBlob = (-1,-1)
    if (column >= 0) {
      var hasNotPlaced = true
      var y = gameBoard.size - 1
      while (hasNotPlaced && y >= 0) {
        if (gameBoard(y)(column) == 0) {
          gameBoard(y)(column) = turn
          newBlob = (column,y)
          hasNotPlaced = false
        }
        y -= 1
      }
      if (!hasNotPlaced) {
        if (turn == 1) playerTurn = 2 else playerTurn = 1
      }
    }
    newBlob
  }
  
  
  private def isSame( cords: (Int,Int), checking: (Int,Int)) = { //används i nästa def
    checking._2 >= 0 && checking._2 <= gameBoard(0).size - 1 && checking._1 >= 0 && checking._1 <= gameBoard.size - 1 &&
    gameBoard(checking._2)(checking._1) == gameBoard(cords._2)(cords._1)
  }
  //Denna används för att kolla om det åt något håll skulle finnas 4 i rad av samma sort dvs om någon vunnit
  def checkIfFour(cords: (Int, Int)): Boolean = {
    var result = false
    var neighbors = Buffer[(Int, Int)]()
    for (neigbor <- directions) {
      val neigborCords = (cords._1 + neigbor._1, cords._2 + neigbor._2)
      if (isSame(cords, neigborCords)) {
        for (neighbor <- directions.tail) {
          var checking = (neigborCords._1 + neighbor._1, neigborCords._2 + neighbor._2)
          var counter = 0
          while (isSame(cords, checking) && counter < 4) {
            counter = counter + 1
            checking = (neighbor._1 + checking._1, neighbor._2 + checking._2)
          }
          if (counter == 3) result = true
        }
      }
    }
    result
  }



}