

import org.scalatest._

class ThresholdSuite extends FunSuite with Matchers {
  test("KMeans2Groups"){
    val items = List(1,2,3,7,8,9)
    val standard_items = Stats.standardScores(items)
    val upper_group_cuttoff = ObjectThresholding.kMeans(2,standard_items) * Stats.standardDeviation(items) + Stats.mean(items)
    val upper_group = items.filter(x => x >= upper_group_cuttoff)
    upper_group shouldBe List(7,8,9)
  }
}