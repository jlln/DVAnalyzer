/**
 * @author james
 */
import org.scalatest._


class ColocSuite extends FunSuite with Matchers{
 
  val offset_test_matrix= List(
   List(0,0,0,0,0),
   List(0,1,2,3,0),
   List(0,2,3,4,0),
   List(0,3,4,5,2)
  )
  
  val test_matrix = List(
   List(0,0,0,0,0f),
   List(0,1,2,3,0f),
   List(0,2,3,4,0f),
   List(0,3,4,5,2f)
  )
  
  val shifted_1x = List(
   List(0,0,0,0,0f),
   List(0,3,1,2,0),
   List(0,4,2,3,0),
   List(0,2,3,4,5)
  )
  
  val shifted_1x_1y = List(
   List(0,0,0,0,0f),
   List(0,2,3,4,0),
   List(0,3,1,2,0),
   List(0,4,2,3,5)
  
  )
  val shifted_2x = List(
   List(0,0,0,0,0),
   List(0,2,3,1,0),
   List(0,3,4,2,0f),
   List(0,5,2,3,4f)
  )
  
  val shifted_2x_1y = List(
   List(0,0,0,0,0f),
   List(0,5,2,3,0f),
   List(0,2,3,1,0f),
   List(0,3,4,2,4f)
  
  )
  test("FindOffsets"){
    Colocalization.findOffsets(offset_test_matrix) shouldBe (List((0,0),(1,1),(1,1),(1,0)),List((0,0),(1,0),(1,0),(1,0),(3,0)))
  }
  
  test("ShiftRow"){
    val test_row_1 = List(0,0,0,1,2,3,4,0,0,0f)
    Colocalization.shiftRow(test_row_1,(3,3),1) shouldBe List(0,0,0,4,1,2,3,0,0,0f)
    Colocalization.shiftRow(test_row_1,(3,3),2) shouldBe List(0,0,0,3,4,1,2,0,0,0f)
    Colocalization.shiftRow(test_row_1,(3,3),3) shouldBe List(0,0,0,2,3,4,1,0,0,0f)
    Colocalization.shiftRow(test_row_1,(3,3),4) shouldBe List(0,0,0,1,2,3,4,0,0,0f)
    Colocalization.shiftRow(test_row_1,(3,3),5) shouldBe Colocalization.shiftRow(test_row_1,(3,3),1)
    val test_row_2 = List(1,2,3,4,0,0,0f)
    Colocalization.shiftRow(test_row_2,(0,3),1) shouldBe List(4,1,2,3,0,0,0f)
    val test_row_3 = List(0,0,0,1,2,3,4f)
    Colocalization.shiftRow(test_row_3,(3,0),1) shouldBe List(0,0,0,4,1,2,3f)
    val test_row_4 = List(0,0,0,1,2,3,4f)
    Colocalization.shiftRow(test_row_3,(3,0),3) shouldBe List(0,0,0,2,3,4,1f)
  }
  
  test("ShiftRows"){
    val offsets = Colocalization.findOffsets(offset_test_matrix)._1
   Colocalization.shiftRows(test_matrix,offsets,1) shouldEqual shifted_1x
  }
  
  test("ShuffleSlice"){
    val offsets = Colocalization.findOffsets(offset_test_matrix)
    Colocalization.shuffleSlice(test_matrix,offsets,1,1) shouldEqual shifted_1x_1y
    Colocalization.shuffleSlice(test_matrix,offsets,2,0) shouldEqual shifted_2x
    Colocalization.shuffleSlice(test_matrix,offsets,2,1) shouldEqual shifted_2x_1y
  }
  
  
  
}