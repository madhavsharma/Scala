/**
 * Description: An autoplayer for TicTacToe. 
 * Does not use any AI (Ideally MinMax kind of approach goes into it).
 * Creating minimilastic features.
 * 
 * Compiled using scala 2.11.6 in Scala IDE Luna ( Ubuntu 15.04) 
 * 
 */

import scala.io.StdIn._
import scala.util.Random
class TicTacTest(var symbol:String) {
      
	 
  var nextChance = symbol
			val mySymbol = if(symbol.equalsIgnoreCase("X") ) "O" else "X"
       //St Board 
				val board = Array(Array("_","_","_"),Array("_","_","_"),Array("_","_","_"))
				//val board = Array(Array("1","2","3"),Array("4","5","6"),Array("7","8","9"))

				def ticTacBoard() = {
					println(board.map(row => println(row(0) + " | " + row(1) + " | " +  row(2))))


			}
   
  /**
   * whoseTurn : Function to know whose turn is it.
   * @Return : String telling whose chance is it, Player's or Computer
   */
   def whoseTurn :String  = nextChance match{ 
     case nextChance:String => { if(nextChance.equals(symbol)) return ("Player's chance") else "Computer's chance" }
     
     }
   
/**
   * move : Function to populate board with moves given by Player and Computer
   * @Return : String telling whose chance is it, Player's or Computer
   * @Param: Array of integers containing x,y co-ordinate of the position
   */
def move( pos: Array[Int]): Boolean = {
				
  if( board(pos(0))(pos(1)).equals("_"))  board(pos(0))(pos(1)) = nextChance  else {println("Already filled slot move to  other position") 
    
  return false} 
					ticTacBoard() 
         nextChance =  if(nextChance.equals(symbol)) mySymbol else symbol
          return true
			}

/**
   * whoWon
   * @Return : String telling status of the game
   * Tells  whether game is Draw, won by player or computer or Game is not over yet 
   */

def whoWon : String = {
   
    def winner(combination : Array[String]) = {
      //println("Checking winner for line "+ combination(0) + "," + combination(1)+ "," + combination(2))
      if(!combination(0).equals("_") && combination(0).equals(combination(1)) && combination(1).equals(combination(2))){ true  }
      else { false  }
    }
  
    def isBoardFull() : Boolean = {
      board.forall { row =>{ row.forall{ element => (!element.equals("_"))}  }  }
     
    }
  
    // Winning combinations
    
    val lineCombinations  = Array(Array(board(0)(0),board(0)(1),board(0)(2)),Array(board(1)(0),board(1)(1),board(1)(2)),
                                 Array(board(2)(0),board(2)(1),board(2)(2)),Array(board(0)(0),board(1)(0),board(2)(0)),
                                 Array(board(0)(1),board(1)(1),board(2)(1)),Array(board(0)(2),board(1)(2),board(2)(2)),
                                 Array(board(0)(0),board(1)(1),board(2)(2)),Array(board(2)(0),board(1)(1),board(0)(2))
        )
   
      lineCombinations.foreach { line =>
      if(winner(line)) { 
        return "WINNER is " + line(0)
      }
    } 
     
  
    //If the board is full it is a tie
    if(isBoardFull()){
      return "Draw"
    }

    return "GameNotOverYet"
  }

/**
   * getPlayerMove : Prompt user for input position and does some sanity checks too
   * @Return: Array[int] containg the co-ordinates of players move
   */
def getPlayerMove = {
 var x_pos = -1
 var y_pos = -1
 
 var playerMove = Array(-1,-1)
 var valid = false
while(!valid){
  //playerMove = readLine("Enter Position To Move : x,y").split(Array(',','(',')')).map(x=> x.toInt)
  playerMove = readLine("Enter Position To Move : x,y").split(Array(',')).map(x=> x.toInt)
 valid =  if(playerMove.forall(x=> (x > -1 &&  x <3))) true else {
   println("got entry : " + playerMove(0)+"," + playerMove(1) +" Enter Position To Move : x,y in proper format and range ")
   false
 }
 

  }
 println ("Player moving " + symbol)
  playerMove
}

/**
   * getComputerMove : Makes computer create his moves on the board
   * @Return: Array[int] containg the co-ordinates of computers move
   */

def getComputerMove : Array[Int] = {

 val rand = new Random
 println ("Computer moving" + mySymbol)
var position = Array(rand.nextInt(3),rand.nextInt(3))
 position
  
}

/**
   * playerAtPosition : Tells for a particular cell if it is empty or filled by either player or computer
   * @Param: Array[int] which is location of the cell 
   * @Return: String indicating the cell owner
   */

def playerAtPosition(pos: Array[Int]):String  =board(pos(0))(pos(1)) match {
  case "_"=> "No One at this position"
  case input:String => { if(input.equals(symbol)) return ("Player at this pos") else "Computer's chance" }
  
  
}

}



object TicTacTesT extends App {
  
  print("Enter the Symbol: ")
  val playerSymbol = readLine()
  println("You chose Symbol : " + playerSymbol)
	val game = new TicTacTest(playerSymbol)
  
 
  while(game.whoWon.equals("GameNotOverYet")){
   
    println(game.whoseTurn )
    game.ticTacBoard()
 
  var success = false

do{
  var position = if(game.nextChance.equals(game.symbol)) game.getPlayerMove else game.getComputerMove
  
  success = game.move(Array(position(0),position(1)))
}while(!success)
 //println(game.playerAtPosition(Array(1,1)))
 
}
  //print status of the game like symbol of the winner, Draw state etc.
  println("Game Status : " + game.whoWon)
}

