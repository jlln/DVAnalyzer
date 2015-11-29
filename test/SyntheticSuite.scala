

/**
 * @author james

 */
import org.scalatest._
class SyntheticSuite extends FunSuite with Matchers{
  
  val test_array = Array(
    Array(0,0,0,0,0,0),
    Array(0,1,0,0,0,1),
    Array(0,1,1,1,0,0),
    Array(1,1,0,0,1,0),
    Array(0,0,0,0,0,0),
    Array(0,1,0,1,0,1)
  )
    val test_array_2 = Array(
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1)
  )
     val test_array_3 = Array(
    Array(2,0,1,0,0,0),
    Array(0,0,0,0,0,1),
    Array(0,1,1,1,0,0),
    Array(1,1,0,0,1,0),
    Array(0,0,0,0,0,0),
    Array(0,1,0,1,0,1)
  )
  
  
    val test_array_4 = Array(
    Array(2,0,1,1,1,1),
    Array(0,0,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1)
  )
  
  val test_array_5 = Array(
    Array(2,0,0,0,1,1),
    Array(0,0,0,0,1,1),
    Array(0,0,0,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1)
  )
  
  val test_array_6 = Array(
    Array(2,2,2,0,1,1),
    Array(2,2,0,0,1,1),
    Array(0,0,0,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1),
    Array(1,1,1,1,1,1)
  )
  
  test("isPixelAvailable1 - available"){
    Synthetics.isPixelAvailable(0,0,test_array,1) shouldBe true
  }
  test("isPixelAvailable2 - flanked by other object pixels"){
    Synthetics.isPixelAvailable(0,0,test_array,2) shouldBe false
  }
  test("isPixelAvailable3 - pixel already part of object"){
    Synthetics.isPixelAvailable(1,1,test_array,1) shouldBe false
  }
  test("getEmptyNeighbours"){
    Synthetics.getEmptyNeighbours(1, 1, test_array,1).sorted shouldBe List( (0,0),(1,0),(0,1),(0,2),(2,0),(2,1)).sorted
  }
  
  test("ExpandBlob1"){
    Synthetics.expandBlob(test_array, 1, 36) shouldBe test_array_2
  }
  
  test("ExpandBlob2"){
    Synthetics.expandBlob(test_array_3, 1, 32) shouldBe test_array_4
  }
  
  test("ExpandBlob3"){
    Synthetics.expandBlob(test_array_3, 2, 32) shouldBe test_array_3
  }
  
  test("ExpandBlob4"){
    Synthetics.expandBlob(test_array_5, 2, 10) shouldBe test_array_6
  }
  
  test("ProceduralBlobs"){
    val t = Synthetics.proceduralBlobs(20,20,5)
    t.map{
      t=> println(t.toList)
    }
  }
}