import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Program for moving robotic explorers around a grid-like landing area.
  * Accepts input from stdin.
  * Outputs the final coordinates and direction for each explorer to stdout
  * Tested on scala version 2.11.6
  * 
  * Invoke using:
  * scalac Venus.scala
  * scala Venus < input
  *
  * @history	v2 24/12/15	Removed exceptions and return statements
  *		v1 26/11/15	Initial version
  */
object Venus {

	/**
	  * Regexs to validate input
	  */
	private val LandingAreaPattern = """(\d+) (\d+)""".r
	private val ExplorerPositionPattern = """(\d+) (\d+) ([NSEW])""".r
	private val ExplorerInstructionPattern = """([LRM]+)""".r

	/**
	  * Main method
	  */ 
	def main(args: Array[String]) {
		val inputLines = io.Source.stdin.getLines
		val processedExplorers = processInput(inputLines)
		
		processedExplorers match {
			case Left(msg) => Console.println(msg)
			case Right(explorers) => explorers.foreach{ println }
		}
	}
	
	/**
	  * Parses all lines of input and returns a list of Orientation instances
	  * Each Orientation represents an explorer's position after its instructions have been applied to it 
	  *
	  * @param lines Iterator containing the lines read in from stdin
	  */
	def processInput(lines: Iterator[String]): Either[String, List[Orientation]] = {
		
		if (lines.isEmpty) {
			Left("Error: No input lines")
		}
		else {
		
			// processExplorer is invoked on each explorer tuple 
			// and returns the final orientation after all instructions have been applied
			def processExplorer(landingAreaUpperRight: (Int, Int))(explorerData: ((Int, Int, Char), String)) : Either[String, Orientation] = {
			
				var orientation: Orientation = Orientation(explorerData._1)
			
				if (orientation.x < 0 || orientation.x > landingAreaUpperRight._1 ||
					orientation.y < 0 || orientation.y > landingAreaUpperRight._2 ) {
					Left(s"Error: Explorer has initial position ($orientation) which is outside the landing area")
				}
				else {

					// applyInstruction is invoked for each instruction.
					// Each invocation 'moves' the orientation instance and 
					// updates the orientation reference
					def applyInstruction(instruction: Char) {
				
						// can potentially check here whether explorer has moved outside the
						// landing area or has moved to the same point as another explorer
						orientation = orientation.move(instruction)
					}

					explorerData._2.foreach{ applyInstruction }
					Right(orientation)
				}
			}
			
			val nestedResult: Either[String, List[Either[String, Orientation]]] = for {
				landingAreaUpperRight <- getLandingArea(lines.next()).right 
				explorersData <- getExplorersData(lines).right
			} 
			yield {
				explorersData.map{ processExplorer(landingAreaUpperRight) }
			}
		
			// if nestedResult is a Left return it 	
			if (nestedResult.isLeft) {
				Left(nestedResult.left.get)
			}
			else {
				// if nestedResult.right contains a Left return it, otherwise 
				// create a List[Orientation] from nestedResult.right and wrap it
				// in Right
				nestedResult.right.get.collectFirst{ case Left(msg) => Left(msg) }.getOrElse{ 
					Right(nestedResult.right.get.collect{ case Right(ori) => ori } )
				} 
			}		
		}
	}

	/**
	  * Parses the first line of input (upper right coords of landing area)  
	  * and returns a tuple (x, y) 
	  *
	  * @param line the first line read in from stdin
	  */
	def getLandingArea(line: String) : Either[String, (Int, Int)] = {
		line match {
			case LandingAreaPattern(x, y) => Right((x.toInt, y.toInt))
			case line if line.trim.isEmpty => Left("Error: Expected upper right coordinate of landing area but found an empty line")
			case _ => Left(s"Error: Upper right landing area coordinate must be in the format '<x-coord> <y-coord>' but was '$line'")
		}
	}

	/**
	  * Parses remaining lines of input and returns a List of tuples (position_tuple, instructions) 
	  * one tuple for each explorer. 
	  *
	  * @param lines Iterator containing the lines read in from stdin (excluding the first line)
	  */
	def getExplorersData(lines: Iterator[String]) : Either[String, List[((Int, Int, Char), String)] ] = {

		// Use ListBuffer as it is mutable and append operation is O(1) 
		@tailrec def getExplorersDataRec(lines : Iterator[String], parsedData: ListBuffer[((Int, Int, Char), String)] ) : Either[String, ListBuffer[((Int, Int, Char), String)] ] = {
			if (lines.isEmpty) {
				Right(parsedData)
			}
			else {
				getSingleExplorerData(lines) match {
					case Left(msg) => Left(msg)
					case Right(explorerData) => getExplorersDataRec(lines, parsedData += explorerData)
				}
			}
		}

		getExplorersDataRec(lines, ListBuffer[((Int, Int, Char), String)]() ) match {
			case Right(explorersData) => Right(explorersData.toList)
			case Left(msg) => Left(msg)
		}
	}

	/**
	  * Parses the two lines of input corresponding to a single explorer and returns
	  * a single tuple (position_tuple, instructions) 
	  *
	  * @param lines Iterator containing the lines read in from stdin (excluding the first line)
	  */
	def getSingleExplorerData(lines: Iterator[String]) : Either[String, ((Int, Int, Char), String)] = {
		val positionLine = lines.next()
		val parsedPositionLine = parsePositionLine(positionLine)

		parsedPositionLine match {
			case Right(pos) => lines match {
				case lines if lines.isEmpty => Left(s"Error: Missing explorer instructions after line '$positionLine'")
				case _ => 	parseInstructionLine(lines.next()) match {
								case Right(instructions) => Right(pos, instructions)
								case Left(msg) => Left(msg)
							}
			}
			case Left(msg) => Left(msg)
		}
	}

	/**
	  * Parses a string containing the position of an explorer
	  * Returns a position tuple (x-coord, ycoord, orientation)
	  *
	  * @param line line containing an explorer's position
	  */
	def parsePositionLine(line : String) : Either[String, (Int, Int, Char)] = {
		line match {
			case ExplorerPositionPattern(x, y, orientation) => Right((x.toInt, y.toInt, orientation.head))
			case line if line.trim.isEmpty => Left("Error: Expected explorer position but found an empty line")
			case _ => Left(s"Error: Explorer position must be in the format '<x-coord> <y-coord> <orientation>' but was '$line'")
		}		
	}

	/**
	  * Parses a string containing the instructions for an explorer
	  * Returns the instruction string
	  */
	def parseInstructionLine(line : String) : Either[String, String] = {
		line match {
			case ExplorerInstructionPattern(instructions) => Right(instructions)
			case line if line.trim.isEmpty => Left("Error: Expected explorer instructions but found an empty line")
			case _ => Left(s"Error: Explorer instructions must be in the format '<instruction string>' but was '$line'")
		}
	}

	/**
 	  * Contains factory method for Orientation instances
	  */
	object Orientation {

		def apply(initialPosition: (Int, Int, Char)) = {
			initialPosition._3 match {
				case 'N' => new NorthOrientation(initialPosition._1, initialPosition._2)
				case 'S' => new SouthOrientation(initialPosition._1, initialPosition._2)
				case 'E' => new EastOrientation(initialPosition._1, initialPosition._2)
				case 'W' => new WestOrientation(initialPosition._1, initialPosition._2)
			}
		}

	}
	
	/**
	  * Trait representing an explorer's position and orientation.
	  * There is a subclass for each compass direction (orientation).
	  * Each subclass overrides move() to implement movement for its orientation.
	  */
	trait Orientation {
		
		/* Members for x-coord and y-coord */
		def x: Int
		def y: Int

		/* Method for moving an explorer, returns the new orientation */
		def move(instruction: Char) : Orientation

		/* Char indicator for the compass direction */
		def directionIndicator : Char

		override def toString(): String = s"$x $y $directionIndicator"
		
	}

	case class NorthOrientation(x: Int, y: Int) extends Orientation {

		override def directionIndicator = 'N'

		def move(instruction: Char) : Orientation = {
			instruction match {
				case 'M' => new NorthOrientation(x, y+1)
				case 'R' => new EastOrientation(x, y)
				case 'L' => new WestOrientation(x, y)
			}
		}
	}

	case class SouthOrientation(x: Int, y: Int) extends Orientation {

		override def directionIndicator = 'S'

		def move(instruction: Char) : Orientation = {
			instruction match {
				case 'M' => new SouthOrientation(x, y-1)
				case 'R' => new WestOrientation(x, y)
				case 'L' => new EastOrientation(x, y)
			}
		}
	}

	case class EastOrientation(x: Int, y: Int) extends Orientation {
		
		override def directionIndicator = 'E'

		def move(instruction: Char) : Orientation = {
			instruction match {
				case 'M' => new EastOrientation(x+1, y)
				case 'R' => new SouthOrientation(x, y)
				case 'L' => new NorthOrientation(x, y)
			}
		}
	}

	case class WestOrientation(x: Int, y: Int) extends Orientation {

		override def directionIndicator = 'W'

		def move(instruction: Char) : Orientation = {
			instruction match {
				case 'M' => new WestOrientation(x-1, y)
				case 'R' => new NorthOrientation(x, y)
				case 'L' => new SouthOrientation(x, y)
			}
		}
	}	


}

