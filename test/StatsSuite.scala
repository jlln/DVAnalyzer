

/**
 * @author james
 */
import org.scalatest._

class StatsSuite extends FunSuite with Matchers {
  test("SD-1"){
    Stats.mean(List(1,0,1,0,1,0)) shouldBe 0.5
    Stats.standardDeviation(List(1,0,1,0,1,0)) shouldBe 0.5
  }
  
  test("SD-2"){
    val values = Array(1,3,1,3,0,4)
    Stats.mean(values) shouldBe 2
    Stats.variance(values) shouldBe 2
    Stats.standardDeviation(values) shouldBe 1.4142135623730951
    Stats.kurtosis(values) shouldBe 1.4999999999999998
  }

  
  test("StandardScores"){
    val values = List(1,3,1,3)
    Stats.mean(values) shouldBe 2
    Stats.standardDeviation(values) shouldBe 1
    Stats.standardScores(values) shouldBe List(-1,1,-1,1)
  }
  
  test("PearsonCorrelationCoefficient positive"){
    val items_a = List(1,2,3,4)
    Stats.correlationPearson(items_a,items_a) shouldBe 1.0

  }
  test("PearsonCorrelationCoefficient negative"){
     val items_a = List(1,2,3,4)
    val items_b = List(-1,-2,-3,-4)
    Stats.correlationPearson(items_a,items_b) shouldBe -1.0
  }
}