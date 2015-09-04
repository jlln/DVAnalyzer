

/**
 * @author james
 */
object Colocalization {
  def pearsonsPixelCorrelation(items_a:Array[Float],items_b:Array[Float],threshold_type:Int): Double = {
    // The threshold_type value can be 1 or 0, depending on if zero-x or zero-zero pixels are to be discarded. 
    // ie use 0 for standard correlation measurement, 
    // and use 1 for pure spatial intensity correlation in overlapping regions.
    val a_threshold = 10
    val b_threshold = 10
    val items_a_nz = items_a.map (x=>(if (x > a_threshold) 1 else 0))
    val items_b_nz = items_b.map (x=>(if (x > b_threshold) 1 else 0))
    val intersections = items_a_nz.zip(items_b_nz) map {case (a,b)=>a+b}
    val intersection_indices = intersections.zipWithIndex.filter(x => x._1 > threshold_type) map {case (z,i) =>i}
    val nz_items_a = intersection_indices.map (i=>items_a(i))
    val nz_items_b = intersection_indices.map (i=>items_b(i))
    val N = nz_items_a.size
    val z_scores_a = Stats.standardScores(nz_items_a)
    val z_scores_b = Stats.standardScores(nz_items_b)
    val r = (z_scores_a.toList.zip(z_scores_b.toList) map {case (za,zb) => za*zb}).sum/(N)
    r match {
      case x if x.isNaN => 0
      case x => x
    }
  }
  def threeWayPearson(red:Array[Float],green:Array[Float],blue:Array[Float]):Result = {
    val red_green = new ResultEntry("RedGreenPearson",Some(pearsonsPixelCorrelation(red,green,0)))
    val green_blue = new ResultEntry("GreenBluePearson",Some(pearsonsPixelCorrelation(green,blue,0)))
    val red_blue = new ResultEntry("RedBluePearson",Some(pearsonsPixelCorrelation(red,blue,0)))
    new Result(red.length,List(red_green,green_blue,red_blue))
  }
}