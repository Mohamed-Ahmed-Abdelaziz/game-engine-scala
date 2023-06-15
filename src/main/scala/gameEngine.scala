import jdk.internal.util.xml.impl.Input
import scala.math._
import scala.io.AnsiColor._

object Main {
  def main(args: Array[String]) {
    val checkersBoard = Array(
      Array('-', 'O', '-', 'O', '-', 'O', '-', 'O'),
      Array('O', '-', 'O', '-', 'O', '-', 'O', '-'),
      Array('-', 'O', '-', 'O', '-', 'O', '-', 'O'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('o', '-', 'o', '-', 'o', '-', 'o', '-'),
      Array('-', 'o', '-', 'o', '-', 'o', '-', 'o'),
      Array('o', '-', 'o', '-', 'o', '-', 'o', '-'),
    )
    val chessBoard = Array(
      Array('R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R'),
      Array('P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('-', '-', '-', '-', '-', '-', '-', '-'),
      Array('p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'),
      Array('r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'),
    )
    val connect4Board = Array(
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o'),
      Array('o', 'o', 'o','o', 'o', 'o','o', 'o')
    )
    val tictactocBoard = Array(Array(' ', ' ', ' '), Array(' ', ' ', ' '), Array(' ', ' ', ' '))
    var rowNumber = Array(7, 7, 7, 7, 7, 7, 7, 7)
    val sudukuBoard = Array(
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-', '-'))
    val queen8Board = Array(
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-'),
      Array('-', '-', '-','-', '-', '-','-', '-')
    )

    println("[1- chess  2- checkers  3- connect4  4- tictactoc  5- suduku  6- 8queen]")
    print("game number: ")
    val gameNumber = scala.io.StdIn.readInt()
    var playerNumber = 1
    var boardC : Array[Array[Char]] = Array(Array(' '))
    if(gameNumber == 1){
      boardC = chessBoard
      gameEngine(chessController, chessDrawer)
    }
    else if(gameNumber == 2){
      boardC = checkersBoard
      gameEngine(checkersController, checkersDrawer)
    }
    else if(gameNumber == 3){
      boardC = connect4Board
      gameEngine(connect4Controller, connect4Drawer)
    }
    else if(gameNumber == 4){
      boardC = tictactocBoard
      gameEngine(tictactocController, tictactocDrawer)
    }
    else if(gameNumber == 5){
      boardC = sudukuBoard
      gameEngine(sudukuController, sudukuDrawer)
    }
    else if(gameNumber == 6){
      boardC = queen8Board
      gameEngine(queen8Controller, queen8Drawer)
    }
    def gameEngine(controller : (Array[Array[Char]], Int, String) => Array[Any],
                   drawer : (Array[Array[Char]]) => Unit): Unit ={
      while (true){
        drawer(boardC)
        print("player " + playerNumber.toString + " turn: ")
        val input = scala.io.StdIn.readLine()
        val returnedState = controller(boardC, playerNumber, input)
        boardC = returnedState(0).asInstanceOf[Array[Array[Char]]]
        val flag = returnedState(1).asInstanceOf[Int]
        if(flag == 0){
          if(playerNumber == 1) playerNumber = 2
          else playerNumber = 1
        }else println("--- Invalid input ---")
      }
    }
    // --------------------- checkers -----------------------
    def checkersDrawer(board: Array[Array[Char]]): Unit = {
      var color = BLACK
      println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(8 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          if(board(i)(j) == 'O' || board(i)(j) == 'Q') {
            color = BLACK
          }else if(board(i)(j) == 'o' || board(i)(j) == 'q') {
            color = WHITE
          }
          print("|  ")
          if(board(i)(j) == '-'){
            print(" ")
          }else{
            var item = board(i)(j).toString
            print(s"${BOLD}${color}" + item + s"${RESET}")
          }
          print("  ")

        }
        println("|")
        println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      }
      println("      a     b     c     d     e     f     g     h")
    }
    def checkersController(board: Array[Array[Char]],player: Int, input: String): Array[Any] = {
      def getIndex(x: String): (Int, Int) = x match {
        case "1a" => (7, 0)
        case "2a" => (6, 0)
        case "3a" => (5, 0)
        case "4a" => (4, 0)
        case "5a" => (3, 0)
        case "6a" => (2, 0)
        case "7a" => (1, 0)
        case "8a" => (0, 0)

        case "1b" => (7, 1)
        case "2b" => (6, 1)
        case "3b" => (5, 1)
        case "4b" => (4, 1)
        case "5b" => (3, 1)
        case "6b" => (2, 1)
        case "7b" => (1, 1)
        case "8b" => (0, 1)

        case "1c" => (7, 2)
        case "2c" => (6, 2)
        case "3c" => (5, 2)
        case "4c" => (4, 2)
        case "5c" => (3, 2)
        case "6c" => (2, 2)
        case "7c" => (1, 2)
        case "8c" => (0, 2)

        case "1d" => (7, 3)
        case "2d" => (6, 3)
        case "3d" => (5, 3)
        case "4d" => (4, 3)
        case "5d" => (3, 3)
        case "6d" => (2, 3)
        case "7d" => (1, 3)
        case "8d" => (0, 3)

        case "1e" => (7, 4)
        case "2e" => (6, 4)
        case "3e" => (5, 4)
        case "4e" => (4, 4)
        case "5e" => (3, 4)
        case "6e" => (2, 4)
        case "7e" => (1, 4)
        case "8e" => (0, 4)

        case "1f" => (7, 5)
        case "2f" => (6, 5)
        case "3f" => (5, 5)
        case "4f" => (4, 5)
        case "5f" => (3, 5)
        case "6f" => (2, 5)
        case "7f" => (1, 5)
        case "8f" => (0, 5)

        case "1g" => (7, 6)
        case "2g" => (6, 6)
        case "3g" => (5, 6)
        case "4g" => (4, 6)
        case "5g" => (3, 6)
        case "6g" => (2, 6)
        case "7g" => (1, 6)
        case "8g" => (0, 6)

        case "1h" => (7, 7)
        case "2h" => (6, 7)
        case "3h" => (5, 7)
        case "4h" => (4, 7)
        case "5h" => (3, 7)
        case "6h" => (2, 7)
        case "7h" => (1, 7)
        case "8h" => (0, 7)

        case _ => (-1, -1)
      }
      val mustMovesPositions = Array(
        Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1), Array(-1, -1),Array(-1, -1)
      )
      val removedPiecesPositions = Array(
        Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1),Array(-1, -1), Array(-1, -1),Array(-1, -1)
      )
      def playerHasMustMoves(board: Array[Array[Char]],player: Int): Int = {
        var numberOfPositioins = 0
        if(player == 2){
          for(i <- 0 to 7){
            for(j <- 0 to 7){
              if(board(i)(j) == 'O' && i < 6){
                if(j == 0 || j == 1){
                  if( (board(i+1)(j+1) == 'o' || board(i+1)(j+1) == 'q') && board(i+2)(j+2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i+2, j+2)
                    removedPiecesPositions(numberOfPositioins) = Array(i+1, j+1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }else if(j == 6 || j == 7){
                  if((board(i+1)(j-1) == 'o' || board(i+1)(j-1) == 'q') && board(i+2)(j-2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i+2, j-2)
                    removedPiecesPositions(numberOfPositioins) = Array(i+1, j-1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }else{
                  if((board(i+1)(j+1) == 'o' || board(i+1)(j+1) == 'q') && board(i+2)(j+2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i+2, j+2)
                    removedPiecesPositions(numberOfPositioins) = Array(i+1, j+1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                  if((board(i+1)(j-1) == 'o' || board(i+1)(j-1) == 'q') && board(i+2)(j-2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i+2, j-2)
                    removedPiecesPositions(numberOfPositioins) = Array(i+1, j-1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }
              }else if(board(i)(j) == 'Q'){
                if(j == 0 || j == 1){
                  if(i < 6) {
                    if ((board(i + 1)(j + 1) == 'o' || board(i + 1)(j + 1) == 'q') && board(i + 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j + 1) == 'o' || board(i - 1)(j + 1) == 'q') && board(i - 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }else if(j == 6 || j == 7){
                  if(i < 6) {
                    if ((board(i + 1)(j - 1) == 'o' || board(i + 1)(j - 1) == 'q') && board(i + 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j - 1) == 'o' || board(i - 1)(j - 1) == 'q') && board(i - 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }else{
                  if(i < 6) {
                    if ((board(i + 1)(j + 1) == 'o' || board(i + 1)(j + 1) == 'q') && board(i + 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                    if ((board(i + 1)(j - 1) == 'o' || board(i + 1)(j - 1) == 'q') && board(i + 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j + 1) == 'o' || board(i - 1)(j + 1) == 'q') && board(i - 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                    if ((board(i - 1)(j - 1) == 'o' || board(i - 1)(j - 1) == 'q') && board(i - 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }
              }
            }
          }
        }else if(player == 1){
          for(i <- 0 to 7){
            for(j <- 0 to 7){
              if(board(i)(j) == 'o' && i > 1){
                if(j == 0 || j == 1){
                  if((board(i-1)(j+1) == 'O' || board(i-1)(j+1) == 'Q') && board(i-2)(j+2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i-2, j+2)
                    removedPiecesPositions(numberOfPositioins) = Array(i-1, j+1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }else if(j == 6 || j == 7){
                  if((board(i-1)(j-1) == 'O' || board(i-1)(j-1) == 'Q') && board(i-2)(j-2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i-2, j-2)
                    removedPiecesPositions(numberOfPositioins) = Array(i-1, j-1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }else{
                  if((board(i-1)(j+1) == 'O' || board(i-1)(j+1) == 'Q') && board(i-2)(j+2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i-2, j+2)
                    removedPiecesPositions(numberOfPositioins) = Array(i-1, j+1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                  if((board(i-1)(j-1) == 'O' || board(i-1)(j-1) == 'Q') && board(i-2)(j-2) == '-'){
                    mustMovesPositions(numberOfPositioins) = Array(i-2, j-2)
                    removedPiecesPositions(numberOfPositioins) = Array(i-1, j-1)
                    numberOfPositioins = numberOfPositioins + 1
                  }
                }
              }else if(board(i)(j) == 'q'){
                if(j == 0 || j == 1){
                  if(i < 6) {
                    if ((board(i + 1)(j + 1) == 'O' || board(i + 1)(j + 1) == 'Q') && board(i + 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j + 1) == 'O' || board(i - 1)(j + 1) == 'Q') && board(i - 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }else if(j == 6 || j == 7){
                  if(i < 6) {
                    if ((board(i + 1)(j - 1) == 'O' || board(i + 1)(j - 1) == 'Q') && board(i + 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j - 1) == 'O' || board(i - 1)(j - 1) == 'Q') && board(i - 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }else{
                  if(i < 6) {
                    if ((board(i + 1)(j + 1) == 'O' || board(i + 1)(j + 1) == 'Q') && board(i + 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                    if ((board(i + 1)(j - 1) == 'O' || board(i + 1)(j - 1) == 'Q') && board(i + 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i + 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i + 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                  if(i > 1) {
                    if ((board(i - 1)(j + 1) == 'O' || board(i - 1)(j + 1) == 'Q') && board(i - 2)(j + 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j + 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j + 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                    if ((board(i - 1)(j - 1) == 'O' || board(i - 1)(j - 1) == 'Q') && board(i - 2)(j - 2) == '-') {
                      mustMovesPositions(numberOfPositioins) = Array(i - 2, j - 2)
                      removedPiecesPositions(numberOfPositioins) = Array(i - 1, j - 1)
                      numberOfPositioins = numberOfPositioins + 1
                    }
                  }
                }
              }
            }
          }
        }
        return numberOfPositioins
      }

      val (sourseRowIndex, sourseColIndex) : (Int, Int) = getIndex(input.substring(0, 2))
      val (destinationRowIndex, destinationColIndex) : (Int, Int) = getIndex(input.substring(2, 4))

      if(sourseRowIndex == -1 || destinationColIndex == -1) return Array(board, 1)
      if(board(destinationRowIndex)(destinationColIndex) != '-') return Array(board, 1)
      if(player == 1){
        if(board(sourseRowIndex)(sourseColIndex) != 'o' && board(sourseRowIndex)(sourseColIndex) != 'q') return Array(board, 1)
      }else if(player == 2){
        if(board(sourseRowIndex)(sourseColIndex) != 'O' && board(sourseRowIndex)(sourseColIndex) != 'Q') return Array(board, 1)
      }
      var numberMustMoves = playerHasMustMoves(board, player)
      if(numberMustMoves > 0) {
        var flage = false
        for (i <- 0 to numberMustMoves) {
          if (mustMovesPositions(i)(0) == destinationRowIndex && mustMovesPositions(i)(1) == destinationColIndex) {
            board(removedPiecesPositions(i)(0))(removedPiecesPositions(i)(1)) = '-'
            flage = true
          }
        }
        if(flage == false) return Array(board, 1)
      }
      else{
        if(player == 1){
          if( !(destinationColIndex == sourseColIndex - 1 && destinationRowIndex == sourseRowIndex - 1) &&
            !(destinationColIndex == sourseColIndex + 1 && destinationRowIndex == sourseRowIndex - 1))  return Array(board, 1)
        }else if(player == 2){
          if( !(destinationColIndex == sourseColIndex - 1 && destinationRowIndex == sourseRowIndex + 1) &&
            !(destinationColIndex == sourseColIndex + 1 && destinationRowIndex == sourseRowIndex + 1))  return Array(board, 1)
        }
      }

      board(destinationRowIndex)(destinationColIndex) = board(sourseRowIndex)(sourseColIndex)
      board(sourseRowIndex)(sourseColIndex) = '-'
      if(player == 1 && destinationRowIndex == 0) board(destinationRowIndex)(destinationColIndex) = 'q'
      if(player == 2 && destinationRowIndex == 7) board(destinationRowIndex)(destinationColIndex) = 'Q'
      return Array(board, 0)
    }

    // -------------- chess -------------------
    def chessDrawer(board: Array[Array[Char]]): Unit ={
      var color = BLACK
      println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(8 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          if(board(i)(j) == 'P' || board(i)(j) == 'R' || board(i)(j) == 'N'
            || board(i)(j) == 'B' || board(i)(j) == 'K' || board(i)(j) == 'Q') {
            color = BLACK
          }else if(board(i)(j) == 'p' || board(i)(j) == 'r' || board(i)(j) == 'n'
            || board(i)(j) == 'b' || board(i)(j) == 'k' || board(i)(j) == 'q') {
            color = WHITE
          }
          print("|  ")
          if(board(i)(j) == '-'){
            print(" ")
          }else{
            var item = board(i)(j).toString
            print(s"${BOLD}${color}" + item + s"${RESET}")
          }
          print("  ")

        }
        println("|")
        println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      }
      println("      a     b     c     d     e     f     g     h")
    }
    def chessController(board: Array[Array[Char]],player: Int, input: String): Array[Any] = {
      def getIndex(x: String): (Int, Int) = x match {
        case "1a" => (7, 0)
        case "2a" => (6, 0)
        case "3a" => (5, 0)
        case "4a" => (4, 0)
        case "5a" => (3, 0)
        case "6a" => (2, 0)
        case "7a" => (1, 0)
        case "8a" => (0, 0)

        case "1b" => (7, 1)
        case "2b" => (6, 1)
        case "3b" => (5, 1)
        case "4b" => (4, 1)
        case "5b" => (3, 1)
        case "6b" => (2, 1)
        case "7b" => (1, 1)
        case "8b" => (0, 1)

        case "1c" => (7, 2)
        case "2c" => (6, 2)
        case "3c" => (5, 2)
        case "4c" => (4, 2)
        case "5c" => (3, 2)
        case "6c" => (2, 2)
        case "7c" => (1, 2)
        case "8c" => (0, 2)

        case "1d" => (7, 3)
        case "2d" => (6, 3)
        case "3d" => (5, 3)
        case "4d" => (4, 3)
        case "5d" => (3, 3)
        case "6d" => (2, 3)
        case "7d" => (1, 3)
        case "8d" => (0, 3)

        case "1e" => (7, 4)
        case "2e" => (6, 4)
        case "3e" => (5, 4)
        case "4e" => (4, 4)
        case "5e" => (3, 4)
        case "6e" => (2, 4)
        case "7e" => (1, 4)
        case "8e" => (0, 4)

        case "1f" => (7, 5)
        case "2f" => (6, 5)
        case "3f" => (5, 5)
        case "4f" => (4, 5)
        case "5f" => (3, 5)
        case "6f" => (2, 5)
        case "7f" => (1, 5)
        case "8f" => (0, 5)

        case "1g" => (7, 6)
        case "2g" => (6, 6)
        case "3g" => (5, 6)
        case "4g" => (4, 6)
        case "5g" => (3, 6)
        case "6g" => (2, 6)
        case "7g" => (1, 6)
        case "8g" => (0, 6)

        case "1h" => (7, 7)
        case "2h" => (6, 7)
        case "3h" => (5, 7)
        case "4h" => (4, 7)
        case "5h" => (3, 7)
        case "6h" => (2, 7)
        case "7h" => (1, 7)
        case "8h" => (0, 7)

        case _ => (-1, -1)
      }
      val player1Pieces = Array('r', 'n', 'b', 'q', 'k', 'p')
      val player2Pieces = Array('R', 'N', 'B', 'Q', 'K', 'P')
      val (sourseRowIndex, sourseColIndex) : (Int, Int) = getIndex(input.substring(0, 2))
      val (destinationRowIndex, destinationColIndex) : (Int, Int) = getIndex(input.substring(2, 4))

      def isValidNightMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(abs(destinatioinRow - sourceRow) == 1 && abs(destinationCol - sourceCol) == 2) return true
        if(abs(destinatioinRow - sourceRow) == 2 && abs(destinationCol - sourceCol) == 1) return true
        return false
      }
      def isValidRookMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(destinatioinRow < sourceRow && destinationCol == sourceCol){
          for(i <- destinatioinRow+1 to sourceRow-1){
            if(board(i)(sourceCol) != '-') return false
          }
        }
        else if(destinatioinRow == sourceRow && destinationCol > sourceCol){
          for(i <- sourceCol+1 to destinationCol-1){
            if(board(sourceRow)(i) != '-') return false
          }
        }
        else if(destinatioinRow > sourceRow && destinationCol == sourceCol){
          for(i <- sourceRow+1 to destinatioinRow-1){
            if(board(i)(sourceCol) != '-') return false
          }
        }
        else if(destinatioinRow == sourceRow && destinationCol < sourceCol){
          for(i <- destinationCol+1 to sourceCol-1){
            if(board(sourceRow)(i) != '-') return false
          }
        }
        else return false

        return true
      }

      def isValidBishopMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(abs(destinatioinRow - sourceRow) != abs(destinationCol - sourceCol)) return false
        var counter: Int = 0
        if(destinatioinRow < sourceRow && destinationCol > sourceCol){
          counter = sourceCol
          for(i <- destinatioinRow+1 to sourceRow-1){
            if(board(i)(counter) != '-') return false
            counter = counter + 1
          }
        }
        else if(destinatioinRow > sourceRow && destinationCol > sourceCol){
          counter = sourceRow
          for(i <- sourceCol+1 to destinationCol-1){
            if(board(counter)(i) != '-') return false
            counter = counter + 1
          }
        }
        else if(destinatioinRow > sourceRow && destinationCol < sourceCol){
          counter = destinationCol
          for(i <- sourceRow+1 to destinatioinRow-1){
            if(board(i)(sourceCol) != '-') return false
            counter = counter + 1
          }
        }
        else if(destinatioinRow < sourceRow && destinationCol < sourceCol){
          counter = destinatioinRow
          for(i <- destinationCol+1 to sourceCol-1){
            if(board(counter)(i) != '-') return false
            counter = counter + 1
          }
        }
        return true
      }

      def isValidQueenMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(isValidRookMove(sourceRow, sourceCol, destinatioinRow, destinationCol) ||
          isValidBishopMove(sourceRow, sourceCol, destinatioinRow, destinationCol)) return true
        return false
      }

      def isValidKingMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(abs(destinatioinRow - sourceRow) + abs(destinationCol - sourceCol) == 1 ) return true
        if(abs(destinatioinRow - sourceRow) == 1 && abs(destinationCol - sourceCol) == 1) return true
        return false
      }

      def isValidWhitePawnMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(destinatioinRow == sourceRow - 1 && destinationCol == sourceCol &&
          board(destinatioinRow)(destinationCol) == '-') return true
        if(destinatioinRow == sourceRow - 1 && abs(destinationCol - sourceCol) == 1 &&
          player2Pieces.contains( board(destinatioinRow)(destinationCol)) ) return true
        return false
      }

      def isValidBlackPawnMove(sourceRow: Int, sourceCol: Int, destinatioinRow: Int, destinationCol:Int): Boolean = {
        if(destinatioinRow == sourceRow + 1 && destinationCol == sourceCol &&
          board(destinatioinRow)(destinationCol) == '-') return true
        if(destinatioinRow == sourceRow + 1 && abs(destinationCol - sourceCol) == 1 &&
          player1Pieces.contains( board(destinatioinRow)(destinationCol)) ) return true
        return false
      }
      def validMove(sourcePiece : Char): Boolean = sourcePiece match {
        case ('r' | 'R') => isValidRookMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case ('n' | 'N') => isValidNightMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case ('b' | 'B') => isValidBishopMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case ('q' | 'Q') => isValidQueenMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case ('k' | 'K') => isValidKingMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case 'p' => isValidWhitePawnMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)

        case 'P' => isValidBlackPawnMove(sourseRowIndex, sourseColIndex, destinationRowIndex, destinationColIndex)
      }
      def isFirstMove(board: Array[Array[Char]]): Boolean ={
        var flag2 = 1
        for(i <- 0 to 7){
          if(board(6)(i) != 'p') flag2 = 0
        }
        if(flag2 == 1) return true
        flag2 = 1
        for(i <- 0 to 7){
          if(board(1)(i) != 'P') flag2 = 0
        }
        if(flag2 == 1) return true
        return false
      }

      if(sourseRowIndex == -1 || destinationColIndex == -1) return Array(board, 1)
      if(isFirstMove(board) && ( (board(sourseRowIndex)(sourseColIndex) != 'p') && (board(sourseRowIndex)(sourseColIndex) != 'P') ) ) return Array(board, 1)
      if(player == 1){
        if(! player1Pieces.contains(board(sourseRowIndex)(sourseColIndex))) return Array(board, 1)
        if(player1Pieces.contains(board(destinationRowIndex)(destinationColIndex))) return Array(board, 1)
      }else if(player == 2){
        if(! player2Pieces.contains(board(sourseRowIndex)(sourseColIndex))) return Array(board, 1)
        if(player2Pieces.contains(board(destinationRowIndex)(destinationColIndex))) return Array(board, 1)
      }
      if(! validMove(board(sourseRowIndex)(sourseColIndex))) return Array(board, 1)

      board(destinationRowIndex)(destinationColIndex) = board(sourseRowIndex)(sourseColIndex)
      board(sourseRowIndex)(sourseColIndex) = '-'
      return Array(board, 0)
    }

    // ------------------ suduku ----------------------------
    def sudukuDrawer(board: Array[Array[Char]]): Unit ={
      println("   |-----+-----+-----+-----+-----+-----+-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(9 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          print("|  ")
          if(board(i)(j) == '-') print(" ")
          else print(board(i)(j))
          print("  ")

        }
        println("|")
        println("   |-----+-----+-----+-----+-----+-----+-----+-----+-----|")
      }
      println("      a     b     c     d     e     f     g     h     i")
    }

    def sudukuController(board: Array[Array[Char]],player: Int, input: String): Array[Any] ={
      def getIndex(x: String): (Int, Int) = x match {
        case "1a" => (8, 0)
        case "2a" => (7, 0)
        case "3a" => (6, 0)
        case "4a" => (5, 0)
        case "5a" => (4, 0)
        case "6a" => (3, 0)
        case "7a" => (2, 0)
        case "8a" => (1, 0)
        case "9a" => (0, 0)

        case "1b" => (8, 1)
        case "2b" => (7, 1)
        case "3b" => (6, 1)
        case "4b" => (5, 1)
        case "5b" => (4, 1)
        case "6b" => (3, 1)
        case "7b" => (2, 1)
        case "8b" => (1, 1)
        case "9b" => (0, 1)

        case "1c" => (8, 2)
        case "2c" => (7, 2)
        case "3c" => (6, 2)
        case "4c" => (5, 2)
        case "5c" => (4, 2)
        case "6c" => (3, 2)
        case "7c" => (2, 2)
        case "8c" => (1, 2)
        case "9c" => (0, 2)

        case "1d" => (8, 3)
        case "2d" => (7, 3)
        case "3d" => (6, 3)
        case "4d" => (5, 3)
        case "5d" => (4, 3)
        case "6d" => (3, 3)
        case "7d" => (2, 3)
        case "8d" => (1, 3)
        case "8d" => (0, 3)

        case "1e" => (8, 4)
        case "2e" => (7, 4)
        case "3e" => (6, 4)
        case "4e" => (5, 4)
        case "5e" => (4, 4)
        case "6e" => (3, 4)
        case "7e" => (2, 4)
        case "8e" => (1, 4)
        case "9e" => (0, 4)

        case "1f" => (8, 5)
        case "2f" => (7, 5)
        case "3f" => (6, 5)
        case "4f" => (5, 5)
        case "5f" => (4, 5)
        case "6f" => (3, 5)
        case "7f" => (2, 5)
        case "8f" => (1, 5)
        case "9f" => (0, 5)

        case "1g" => (8, 6)
        case "2g" => (7, 6)
        case "3g" => (6, 6)
        case "4g" => (5, 6)
        case "5g" => (4, 6)
        case "6g" => (3, 6)
        case "7g" => (2, 6)
        case "8g" => (1, 6)
        case "9g" => (0, 6)

        case "1h" => (8, 7)
        case "2h" => (7, 7)
        case "3h" => (6, 7)
        case "4h" => (5, 7)
        case "5h" => (4, 7)
        case "6h" => (3, 7)
        case "7h" => (2, 7)
        case "8h" => (1, 7)
        case "9h" => (0, 7)

        case "1i" => (8, 8)
        case "2i" => (7, 8)
        case "3i" => (6, 8)
        case "4i" => (5, 8)
        case "5i" => (4, 8)
        case "6i" => (3, 8)
        case "7i" => (2, 8)
        case "8i" => (1, 8)
        case "9i" => (0, 8)

        case _ => (-1, -1)
      }
      def checkValidityOfPosition(board: Array[Array[Char]], rowIndex: Int, colIndex: Int, num: Int): Boolean = {
        val slice1 = Array(0, 1, 2)
        val slice2 = Array(3, 4, 5)
        val slice3 = Array(6, 7, 8)
        var flag = true
        //   check validity in mini square
        if(slice1.contains(rowIndex) && slice1.contains(colIndex)){
          slice1.foreach(i => slice1.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice1.contains(rowIndex) && slice2.contains(colIndex)){
          slice1.foreach(i => slice2.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice1.contains(rowIndex) && slice3.contains(colIndex)){
          slice1.foreach(i => slice3.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice2.contains(rowIndex) && slice1.contains(colIndex)){
          slice2.foreach(i => slice1.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice2.contains(rowIndex) && slice2.contains(colIndex)){
          slice2.foreach(i => slice2.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice2.contains(rowIndex) && slice3.contains(colIndex)){
          slice2.foreach(i => slice3.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice3.contains(rowIndex) && slice1.contains(colIndex)){
          slice3.foreach(i => slice1.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice3.contains(rowIndex) && slice2.contains(colIndex)){
          slice3.foreach(i => slice2.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }
        else if(slice3.contains(rowIndex) && slice3.contains(colIndex)){
          slice3.foreach(i => slice3.foreach(j => if(board(i)(j).toInt == num) flag = false))
        }

        //  check validity in whole board
        for(i <- 0 to 8){
          if(board(i)(colIndex).toInt == num) flag = false
          if(board(rowIndex)(i).toInt == num) flag = false
        }
        return flag
      }
      val (rowIndex, colIndex) : (Int, Int) = getIndex(input.substring(0, 2))
      if(rowIndex == -1) return Array(board, 1)
      if(board(rowIndex)(colIndex) != '-') return Array(board, 1)
      if(input.charAt(2).toInt < 49 || input.charAt(2).toInt > 57) return Array(board, 1)
      if(! checkValidityOfPosition(board, rowIndex, colIndex, input.charAt(2).toInt)) return Array(board, 1)
      board(rowIndex)(colIndex) = input.charAt(2)
      return Array(board, 0)
    }

    // -----------------  8 Queen  ------------------
    def queen8Drawer(board: Array[Array[Char]]): Unit ={
      println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(8 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          print("|  ")
          if(board(i)(j) == '-') print(" ")
          else print(board(i)(j))
          print("  ")

        }
        println("|")
        println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      }
      println("      a     b     c     d     e     f     g     h")
    }

    def queen8Controller(board: Array[Array[Char]],player: Int, input: String): Array[Any] ={
      def test(x: String): (Int, Int) = x match {
        case "1a" => (7, 0)
        case "2a" => (6, 0)
        case "3a" => (5, 0)
        case "4a" => (4, 0)
        case "5a" => (3, 0)
        case "6a" => (2, 0)
        case "7a" => (1, 0)
        case "8a" => (0, 0)

        case "1b" => (7, 1)
        case "2b" => (6, 1)
        case "3b" => (5, 1)
        case "4b" => (4, 1)
        case "5b" => (3, 1)
        case "6b" => (2, 1)
        case "7b" => (1, 1)
        case "8b" => (0, 1)

        case "1c" => (7, 2)
        case "2c" => (6, 2)
        case "3c" => (5, 2)
        case "4c" => (4, 2)
        case "5c" => (3, 2)
        case "6c" => (2, 2)
        case "7c" => (1, 2)
        case "8c" => (0, 2)

        case "1d" => (7, 3)
        case "2d" => (6, 3)
        case "3d" => (5, 3)
        case "4d" => (4, 3)
        case "5d" => (3, 3)
        case "6d" => (2, 3)
        case "7d" => (1, 3)
        case "8d" => (0, 3)

        case "1e" => (7, 4)
        case "2e" => (6, 4)
        case "3e" => (5, 4)
        case "4e" => (4, 4)
        case "5e" => (3, 4)
        case "6e" => (2, 4)
        case "7e" => (1, 4)
        case "8e" => (0, 4)

        case "1f" => (7, 5)
        case "2f" => (6, 5)
        case "3f" => (5, 5)
        case "4f" => (4, 5)
        case "5f" => (3, 5)
        case "6f" => (2, 5)
        case "7f" => (1, 5)
        case "8f" => (0, 5)

        case "1g" => (7, 6)
        case "2g" => (6, 6)
        case "3g" => (5, 6)
        case "4g" => (4, 6)
        case "5g" => (3, 6)
        case "6g" => (2, 6)
        case "7g" => (1, 6)
        case "8g" => (0, 6)

        case "1h" => (7, 7)
        case "2h" => (6, 7)
        case "3h" => (5, 7)
        case "4h" => (4, 7)
        case "5h" => (3, 7)
        case "6h" => (2, 7)
        case "7h" => (1, 7)
        case "8h" => (0, 7)

        case _ => (-1, -1)
      }
      def checkValidityOfPosition(board: Array[Array[Char]], rowIndex: Int, colIndex: Int): Int = {
        for(i <- 0 to 7){
          if(board(i)(colIndex) == 'Q') return 1
          if(board(rowIndex)(i) == 'Q') return 1
        }
        for(i <- 0 to 7){
          for(j <- 0 to 7){
            if( (i - rowIndex) + (j - colIndex) == 0){
              if(board(i)(j) == 'Q') return 1
            }
          }
        }
        return 0
      }
      val (rowIndex, colIndex) : (Int, Int) = test(input)
      if(rowIndex == -1) return Array(board, 1)
      if (board(rowIndex)(colIndex) == '-'){
        if(checkValidityOfPosition(board, rowIndex, colIndex) == 0)
          board(rowIndex)(colIndex) = 'Q'
        return Array(board, 0)
      }else{
        return Array(board, 1)
      }
    }

    // -------------------  connect4 ---------------
    def connect4Drawer(board: Array[Array[Char]]): Unit ={
      var color = RED
      println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(8 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          if(board(i)(j) == 'R') {
            color = RED
          }else if(board(i)(j) == 'Y') {
            color = YELLOW
          }
          print("|  ")
          if(board(i)(j) == 'o'){
            print(" ")
          }else{
            var item = board(i)(j).toString
            print(s"${BOLD}${color}" + item + s"${RESET}")
          }
          print("  ")

        }
        println("|")
        println("   |-----+-----+-----+-----+-----+-----+-----+-----|")
      }
      println("      a     b     c     d     e     f     g     h")
    }
    //    var rowNumber = Array(7, 7, 7, 7, 7, 7, 7, 7)
    def connect4Controller(board: Array[Array[Char]],player: Int, input: String): Array[Any] ={
      //      val rowIndex = input.charAt(0).toInt
      def test(x: String): Int = x match {
        case "a" => 0
        case "b" => 1
        case "c" => 2
        case "d" => 3
        case "e" => 4
        case "f" => 5
        case "g" => 6
        case "h" => 7

        case _ => -1
      }
      val colIndex : Int = test(input)
      if(colIndex == -1) return Array(board, 1)
      if(player == 1){
        if (rowNumber(colIndex) > 0){
          board(rowNumber(colIndex))(colIndex) = 'R'
          rowNumber(colIndex) = rowNumber(colIndex) - 1
          return Array(board, 0)
        }else{
          return Array(board, 1)
        }
      }else{
        if (rowNumber(colIndex) >= 0){
          board(rowNumber(colIndex))(colIndex) = 'Y'
          rowNumber(colIndex) = rowNumber(colIndex) - 1
          return Array(board, 0)
        }else{
          return Array(board, 1)
        }
      }
    }

    // -----------------  tic tac toc  ------------------------
    def tictactocDrawer(board: Array[Array[Char]]): Unit ={
      var color = RED
      println("   |-----+-----+-----|")
      for (i <- 0 until board.length) {
        print(3 - i)
        print("  ")
        for (j <- 0 until board(i).length) {
          if(board(i)(j) == 'x') {
            color = RED
          }else if(board(i)(j) == 'o') {
            color = BLUE
          }
          print("|  ")
          var item = board(i)(j).toString
          print(s"${BOLD}${color}" + item + s"${RESET}")
          print("  ")
        }
        println("|")
        println("   |-----+-----+-----|")
      }
      println("      a     b     c")
    }
    def tictactocController(board: Array[Array[Char]],player: Int, input: String): Array[Any] ={
      def test(x: String): (Int, Int) = x match {
        case "1a" => (2, 0)
        case "2a" => (1, 0)
        case "3a" => (0, 0)

        case "1b" => (2, 1)
        case "2b" => (1, 1)
        case "3b" => (0, 1)

        case "1c" => (2, 2)
        case "2c" => (1, 2)
        case "3c" => (0, 2)

        case _ => (-1, -1)
      }
      val (rowIndex, colIndex) : (Int, Int) = test(input)
      if(rowIndex == -1) return Array(board, 1)
      if(player == 1){
        if (board(rowIndex)(colIndex) == ' '){
          board(rowIndex)(colIndex) = 'x'
          return Array(board, 0)
        }else{
          return Array(board, 1)
        }
      }else{
        if (board(rowIndex)(colIndex) == ' '){
          board(rowIndex)(colIndex) = 'o'
          return Array(board, 0)
        }else{
          return Array(board, 1)
        }
      }

    }

  }
}
