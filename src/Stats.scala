

/**
 * @author james
 */
import scala.math.sqrt
import scala.math.pow
object Stats {
  def mean[T](items:Traversable[T])(implicit n:Numeric[T]) = 
    n.toDouble(items.sum) / items.size.toDouble
  
  
  def sumOfPower[T](items:Traversable[T],power:Int)(implicit n:Numeric[T]):Double = {
    val item_mean = mean(items)
    items.map{
      i=> scala.math.pow((n.toDouble(i)-item_mean),power)
    }.sum
  }

  
  def variance[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {
    val item_mean = mean(items)
    val count = items.size
    val sum_of_squares = sumOfPower(items,2)
    (sum_of_squares/(count-1))
  }
  
  def standardDeviation[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = 
    scala.math.sqrt(variance(items))
  
  def standardScores[T](items:Traversable[T])(implicit n:Numeric[T]) : Traversable[Double] = {
    val sample_mean = mean(items)
    val sample_standard_deviation = standardDeviation(items)
    items map (x => (n.toDouble(x)-sample_mean)/sample_standard_deviation)
  }
  
  def correlationPearson[T](items_a:Traversable[T],items_b:Traversable[T])(implicit n:Numeric[T]):Double = {
    val z_scores_a = standardScores(items_a).toArray
    val z_scores_b = standardScores(items_b).toArray
    val z_scores_ab = z_scores_a.zip(z_scores_b).map{
      case (a,b) => a*b}
    z_scores_ab.sum/(z_scores_ab.length-1) match{
      case x if x.isNaN => 0
      case x => x
    }
  }
  
  def eucledian[T](x1:T,y1:T,x2:T,y2:T)(implicit n:Numeric[T]):Double = {
  
    val dy = n.toDouble(y1) - n.toDouble(y2)
    val dx = n.toDouble(x1) - n.toDouble(x2)
    sqrt(pow(dx,2)+pow(dy,2))
  }
  
  def skewness[T](items:Traversable[T])(implicit n:Numeric[T]):Double = {
    val m = mean(items)
    val s = standardDeviation(items)
    val items_double = items.map{x=>n.toDouble(x)}
    val third_moment = items_double.map{x=>scala.math.pow((x.toDouble-m),3)}
    val summed_normed_third_moment = (third_moment.sum)/items.size.toDouble
    summed_normed_third_moment/scala.math.pow(s,3)
  }

  def kurtosis[T](items:Traversable[T])(implicit n:Numeric[T]):Double = {
    val n_ = items.size
    val s2 = sumOfPower(items,2)
    val s4 = sumOfPower(items,4)
    val m2 = s2/n_
    val m4 = s4/n_
    m4/scala.math.pow(m2,2) -3
    
  }
}