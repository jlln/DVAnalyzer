

/**
 * @author james
 */
import org.scalatest._

class StatsSuite extends FunSuite with Matchers {
  test("SD-1"){
    Stats.mean(List(1,0,1,0,1,0)) shouldBe 0.5
    Stats.standardDeviation(List(1,0,1,0,1,0)) shouldBe (0.54772 +- 0.001)
  }
  
  test("SD-2"){
    val values = Array(1,3,1,3,0,4)
    Stats.mean(values) shouldBe 2
    Stats.variance(values) shouldBe 2.4
    Stats.standardDeviation(values) shouldBe (1.5492 +- 0.0001)
    Stats.kurtosis(values) shouldBe (-1.5 +- 0.00001)
  }

  
  test("StandardScores"){
    val values = List(1,3,1,3)
    Stats.mean(values) shouldBe 2
    Stats.standardDeviation(values) shouldBe (1.1547 +- 0.0001)
  }
  
  test("PearsonCorrelationCoefficient positive"){
    val items_a = List(1,2,3,4)
    Stats.correlationPearson(items_a,items_a) shouldBe Some(1.0)

  }
  test("PearsonCorrelationCoefficient negative"){
    val items_a = List(1,2,3,4)
    val items_b = List(-1,-2,-3,-4)
    Stats.correlationPearson(items_a,items_b) shouldBe Some(-1.0)
  }
  
  test("Kurtosis-2"){
    val d = List(1,2,3,4,5,6,7,8,9)
    Stats.kurtosis(d) shouldBe (-1.23 +- 0.00001)
  }
}