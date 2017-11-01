
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Predef._
import Venus._

object VenusTest {

	def main(args: Array[String]) {
		testParseInstructionLine()
		testGetLandingArea()
		testParsePositionLine()
		testGetSingleExplorerData()
		testGetExplorersData()
		testProcessInput()
		println("All tests passed successfully")
	}

	def testProcessInput() {
		val testData1 = Array[String]("5 5", "1 2 N", "LMLMLMLMM", "3 3 E", "MMRMMRMRRM")
		val result1 = processInput(testData1.iterator)
		val expected1 = Right(List(NorthOrientation(1,3), EastOrientation(5,1)))
		assert(result1.equals(expected1),
			"error in processInput, expected " + expected1 + " but was " + result1)

		val testData2 = Array[String]("10 10", "1 1 N", "MRRMR")
		val result2 = processInput(testData2.iterator)
		val expected2 = Right(List(WestOrientation(1,1)))
		assert(result2.equals(expected2),
			"error in processInput, expected " + expected2 + " but was " + result2)

		val testData3 = Array[String]("5 5", "6 6 N", "MRRMR")
		val result3 = processInput(testData3.iterator)
		val expected3 = Left("Error: Explorer has initial position (6 6 N) which is outside the landing area")
		assert(result3.equals(expected3),
			"error in processInput, expected " + expected3 + " but was " + result3)
	}

	def testGetLandingArea() {
		val res1 = getLandingArea("G ")
		val expected1 = Left("Error: Upper right landing area coordinate must be in the format '<x-coord> <y-coord>' but was 'G '")
		assert(res1.equals(expected1), "error in getLandingArea, expected " + expected1 + " but was " + res1)		
		
		val res2 = getLandingArea("")
		val expected2 = Left("Error: Expected upper right coordinate of landing area but found an empty line")
		assert(res2.equals(expected2), "error in getLandingArea, expected " + expected2 + " but was " + res2)

		val res3 = getLandingArea("6 7")
		val expected3 = Right((6, 7))
		assert(res3.equals(expected3), "error in getLandingArea, expected " + expected3 + " but was " + res3)
	}

	def testGetSingleExplorerData() {
		val res1 = getSingleExplorerData(Array[String]("1 2 N", "LMLM").iterator )
		assert(res1.equals(Right( ((1,2,'N'), "LMLM") )), 
			"error parsing single explorer data, expected Right( ((1,2,\"N\"), \"LMLM\") ) but was " + res1)

		val res2 = getSingleExplorerData(Array[String]("1 2 N", "LMLN").iterator )
		val expected2 = Left("Error: Explorer instructions must be in the format '<instruction string>' but was 'LMLN'")
		assert(res2.equals(expected2), 
			"error parsing single explorer data, expected " + expected2 + " but was " + res2)
	}

	def testParseInstructionLine() {
		val res1 = parseInstructionLine("MLRLRM")
		assert(res1.equals(Right("MLRLRM")), 
			"error parsing instruction line, expected Success(MLRLRM) but was " + res1)

		val res2 = parseInstructionLine("MLRLRP")
		val expected2 = Left("Error: Explorer instructions must be in the format '<instruction string>' but was 'MLRLRP'")
		assert(res2.equals(expected2), 
			"error parsing instruction line, expected " + expected2 + " but was " + res2)
	}

	def testParsePositionLine() {
		val res1 = parsePositionLine("1 2 N")
		assert(res1.equals(Right((1, 2, 'N')) ), 
			"error parsing Position line, expected Right((1, 2, N)) but was " + res1)

		val res2 = parsePositionLine("1 2 3")
		val expected2 = Left("Error: Explorer position must be in the format '<x-coord> <y-coord> <orientation>' but was '1 2 3'")
		assert(res2.equals(expected2), 
			"error parsing Position line, expected " + expected2 + " but was " + res2)

		val res3 = parsePositionLine("1 2 M")
		val expected3 = Left("Error: Explorer position must be in the format '<x-coord> <y-coord> <orientation>' but was '1 2 M'")
		assert(res3.equals(expected3), 
			"error parsing Position line, expected " + expected3 + " but was " + res3)

		val res4 = parsePositionLine("1 2 NN")
		val expected4 = Left("Error: Explorer position must be in the format '<x-coord> <y-coord> <orientation>' but was '1 2 NN'")
		assert(res4.equals(expected4), 
			"error parsing Position line, expected " + expected4 + " but was " + res4)
		//println(res4)
	
	}

	def testGetExplorersData() {
		val testData1 = Array[String]("1 2 N", "LMLM", "3 3 E", "MMRMMR")
		val result1 = getExplorersData(testData1.iterator)
		val expected1 = Right(List( 
				((1, 2, 'N'), "LMLM"),
				((3, 3, 'E'), "MMRMMR")
				 ))
				
		assert(result1.equals(expected1),
			 "error in getExplorerData, expected " + expected1 + " but was " + result1)

		val testData2 = Array[String]("1 2 N", "LMLM", "3 3 E")
		val result2 = getExplorersData(testData2.iterator )
		val expected2 = Left("Error: Missing explorer instructions after line '3 3 E'")
		assert(result2.equals(expected2), 
			"error in getExplorerData, expected " + expected2 + " but was " + result2)

		val testData3 = Array[String]("1 2 N", "LMLM", "3 3 E", "")
		val result3 = getExplorersData(testData3.iterator )
		val expected3 = Left("Error: Expected explorer instructions but found an empty line")
		assert(result3.equals(expected3), 
			"error in getExplorerData, expected " + expected3 + " but was " + result3)
	}

}
