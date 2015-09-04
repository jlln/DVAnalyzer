

/**
 * @author james
 */
import scala.math.sqrt
import scala.math.pow
object Stats {
  def mean[T](items:Traversable[T])(implicit n:Numeric[T]) = 
    n.toDouble(items.sum) / items.size.toDouble
  
  
  def variance[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {
    val itemMean = mean(items)
    val count = items.size
    val sum_of_squares = items.map{
      i=> scala.math.pow(n.toDouble(i),2)
    }.sum
    (sum_of_squares/count) - scala.math.pow(itemMean,2)
  }
  
  def standardDeviation[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = 
    scala.math.sqrt(variance(items))
  
  def standardScores[T](items:Traversable[T])(implicit n:Numeric[T]) : Traversable[Double] = {
    val sample_mean = mean(items)
    val sample_standard_deviation = standardDeviation(items)
    items map (x => (n.toDouble(x)-sample_mean)/sample_standard_deviation)
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
    val m = mean(items)
    val s = standardDeviation(items)
    val items_double = items.map{x=>n.toDouble(x)}
    val third_moment = items_double.map{x=>scala.math.pow((x.toDouble-m),4)}
    val summed_normed_third_moment = (third_moment.sum)/items.size.toDouble
    summed_normed_third_moment/scala.math.pow(s,4)
  }
}