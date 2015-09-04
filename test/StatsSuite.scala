

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
}